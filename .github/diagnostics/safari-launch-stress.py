#!/usr/bin/env python3
# Temporary diagnostic: start Safari WebDriver sessions over and over, through a fresh safaridriver
# each time (launched directly or by Selenium), and record launches that never start listening and
# session requests that stall.
#
# Modes:
#   bare-low  launch safaridriver ourselves on a port in 1024-49151, the range Selenium picks from
#   bare-eph  the same, on a port in 49152-65535, the range haskell-webdriver picks from
#   sel3      Selenium 3.141.59 standalone, which launches /usr/bin/safaridriver per session
#   sel4      Selenium 4 standalone (-Dwebdriver.safari.driver set), per session as well
#   sel4new   the same with a newer Selenium 4 jar
# Each bare iteration stops safaridriver the way Selenium 3 does (SIGTERM, 2 s, SIGKILL) and the
# next one starts at once, so launches follow each other as closely as they do under Selenium.

import argparse
import datetime
import json
import os
import random
import socket
import statistics
import subprocess
import sys
import threading
import time
import urllib.error
import urllib.request

LOG_PREDICATE = ('subsystem BEGINSWITH "com.apple.WebDriver" OR process CONTAINS "WebDriver" '
                 'OR process == "safaridriver" OR eventMessage CONTAINS[c] "webdriver"')


def stamp():
    return datetime.datetime.now().strftime("%H:%M:%S.%f")[:-3]


def say(msg):
    print(f"{stamp()} {msg}", flush=True)


def run(cmd, timeout=60):
    try:
        r = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
        return r.stdout + r.stderr
    except Exception as e:
        return f"<{cmd[0]} failed: {e!r}>\n"


def is_free(host, port):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind((host, port))
        return True
    except OSError:
        return False
    finally:
        s.close()


def pick_port(low):
    for _ in range(200):
        port = random.randint(1024, 49151) if low else random.randint(49152, 65535)
        if is_free("127.0.0.1", port) and is_free("0.0.0.0", port):
            return port
    raise RuntimeError("no free port")


def listening(port):
    try:
        with socket.create_connection(("127.0.0.1", port), timeout=0.5):
            return True
    except OSError:
        return False


def http(method, url, body=None, timeout=60):
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(url, data=data, method=method,
                                 headers={"Content-Type": "application/json; charset=utf-8"})
    t0 = time.monotonic()
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return r.status, r.read().decode(errors="replace"), time.monotonic() - t0
    except urllib.error.HTTPError as e:
        return e.code, e.read().decode(errors="replace"), time.monotonic() - t0
    except Exception as e:
        return None, repr(e), time.monotonic() - t0


def alive(pid):
    try:
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def listener_pid(port):
    pids = [int(x) for x in run(["lsof", "-nP", "-t", f"-iTCP:{port}", "-sTCP:LISTEN"]).split() if x.isdigit()]
    return pids[0] if pids else None


def children(ppid, name):
    kids = []
    for line in run(["ps", "-axo", "pid=,ppid=,comm="]).splitlines():
        parts = line.split(None, 2)
        if len(parts) == 3 and parts[1] == str(ppid) and parts[2].endswith(name):
            kids.append(int(parts[0]))
    return kids


def stop(proc_or_pid):
    """Stop a driver the way Selenium 3 does: SIGTERM, up to 2 s, then SIGKILL. Returns how."""
    if isinstance(proc_or_pid, subprocess.Popen):
        proc = proc_or_pid
        if proc.poll() is not None:
            return f"exited {proc.returncode}"
        proc.terminate()
        try:
            proc.wait(2)
            return f"term {proc.returncode}"
        except subprocess.TimeoutExpired:
            proc.kill()
            try:
                proc.wait(1)
            except subprocess.TimeoutExpired:
                pass
            return "killed"
    pid = proc_or_pid
    try:
        os.kill(pid, 15)
    except ProcessLookupError:
        return "gone"
    for _ in range(40):
        if not alive(pid):
            return "term"
        time.sleep(0.05)
    try:
        os.kill(pid, 9)
    except ProcessLookupError:
        pass
    return "killed"


def snapshot(outdir, label, sample_pids=()):
    path = os.path.join(outdir, "snapshots", f"{label}.txt")
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(f"== {stamp()} {label}\n== processes\n")
        for line in run(["ps", "-axo", "pid,ppid,lstart,etime,stat,args"]).splitlines():
            if any(k in line for k in ("safaridriver", "WebDriver", "java", "PID")):
                f.write(line + "\n")
        f.write("== listeners\n")
        for line in run(["lsof", "-nP", "-iTCP", "-sTCP:LISTEN"]).splitlines():
            if line.startswith(("COMMAND", "com.apple", "safaridri", "java")):
                f.write(line + "\n")
        for pid in sample_pids:
            f.write(f"== sample {pid}\n")
            f.write(run(["sample", str(pid), "2", "-mayDie"], timeout=30))
        f.write("== unified log, last 2 minutes\n")
        f.write(run(["/usr/bin/log", "show", "--last", "2m", "--style", "compact", "--info", "--debug",
                     "--predicate", LOG_PREDICATE], timeout=120))
    # Repeat the gist on stdout, so CI job logs carry it without downloading the artifact.
    text = open(path).read()
    head, _, log = text.partition("== unified log")
    keys = ("Launched", "Server started", "Started HTTP", "termination", "exiting", "Stopping", "rror", "ail",
            "nvalid", "denied", "Unable", "transitioning", "Rejecting", "paired", "crash")
    lines = [l for l in log.splitlines() if ("safaridriver[" in l or "HTTPService[" in l or "Automation]" in l)
             and any(k in l for k in keys)]
    print(head.rstrip() + "\n== unified log, key lines\n" + "\n".join(l[:260] for l in lines[-60:]), flush=True)
    return path


def screen_locked():
    return "CGSSessionScreenIsLocked\"=Yes" in run(["ioreg", "-n", "Root", "-d1"])


def hid_idle_s():
    """Seconds since the last keyboard, mouse or trackpad input."""
    for line in run(["ioreg", "-c", "IOHIDSystem", "-d", "4"]).splitlines():
        if '"HIDIdleTime"' in line:
            return int(line.rsplit("=", 1)[1].strip()) / 1e9
    return None


def automation_disabled_since(start):
    out = run(["/usr/bin/log", "show", "--start", start.strftime("%Y-%m-%d %H:%M:%S"), "--style", "compact",
               "--predicate", 'category == "Automation" AND eventMessage CONTAINS "Remote Automation is now disabled"'],
              timeout=120)
    return "Remote Automation is now disabled" in out


def wait_for_idle(min_idle):
    """Hold off while someone (or a cat) is using the keyboard or mouse, so Safari's automation windows
    don't get input and show the Automation Interruption alert."""
    waited = 0
    while True:
        idle = hid_idle_s()
        if idle is None or idle >= min_idle:
            return waited
        if waited == 0:
            say(f"input {idle:.0f}s ago; waiting for {min_idle:.0f}s without input")
        time.sleep(15)
        waited += 15


def interruptions_since(start):
    out = run(["/usr/bin/log", "show", "--start", start.strftime("%Y-%m-%d %H:%M:%S"), "--style", "compact",
               "--predicate", 'category == "Automation" AND (eventMessage CONTAINS "Interruption" '
                              'OR eventMessage CONTAINS "intercepted")'], timeout=120)
    return [l for l in out.splitlines() if "Interruption" in l or "intercepted" in l]


class Selenium:
    def __init__(self, name, java, jar, major, driver, outdir):
        self.name = name
        self.port = pick_port(False)
        args = [java]
        if major == 4:
            args.append(f"-Dwebdriver.safari.driver={driver}")
        args += ["-jar", jar]
        args += ["-port", str(self.port)] if major == 3 else ["standalone", "--port", str(self.port), "--host", "localhost"]
        self.logpath = os.path.join(outdir, f"selenium-{name}.log")
        self.logfile = open(self.logpath, "w")
        say(f"starting {name}: {' '.join(args)}")
        self.proc = subprocess.Popen(args, stdout=self.logfile, stderr=subprocess.STDOUT)
        self.base = f"http://localhost:{self.port}" + ("/wd/hub" if major == 3 else "")
        deadline = time.monotonic() + 120
        while time.monotonic() < deadline:
            text = open(self.logpath).read()
            if "Selenium Server is up and running" in text or "Started Selenium Standalone" in text:
                say(f"{name} is up on {self.port}")
                return
            if self.proc.poll() is not None:
                break
            time.sleep(0.5)
        raise RuntimeError(f"{name} didn't start; see {self.logpath}")

    def close(self):
        for pid in children(self.proc.pid, "safaridriver"):
            stop(pid)
        stop(self.proc)


def caps(browser):
    return {"capabilities": {"alwaysMatch": {"browserName": browser}}}


def bare_iteration(ctx, mode, rnd, i):
    # bare-overlap stops the previous iteration's safaridriver with SIGTERM just before launching
    # this one, without waiting for it to exit; bare-concurrent leaves the previous one running until
    # this one has listened or failed. The other bare modes stop each safaridriver before moving on.
    port = pick_port(mode == "bare-low")
    held = ctx.pop("held_driver", None)
    if held is not None and mode == "bare-overlap" and held.poll() is None:
        held.terminate()
    rec = {"mode": mode, "round": rnd, "i": i, "t": stamp(), "port": port, "bidi_8080_busy": listening(8080),
           "prev_driver_alive": held is not None and held.poll() is None}
    prev = ctx.get("prev_svc")
    rec["prev_svc_alive"] = prev is not None and alive(prev)
    outpath = os.path.join(ctx["outdir"], "driver-output", f"{mode}-{rnd}-{i}.txt")
    with open(outpath, "w") as out:
        t0 = time.monotonic()
        proc = subprocess.Popen([ctx["safaridriver"], "-p", str(port)], stdout=out, stderr=subprocess.STDOUT)
        up = False
        while time.monotonic() - t0 < ctx["listen_timeout"]:
            if proc.poll() is not None:
                break
            if listening(port):
                up = True
                break
            time.sleep(0.025)
        rec["listen_s"] = round(time.monotonic() - t0, 3)
        rec["listened"] = up
        if held is not None:
            rec["prev_driver_stop"] = stop(held)
        if up and ctx["no_session"]:
            rec["svc_pid"] = ctx["prev_svc"] = listener_pid(port)
        elif up:
            st, body, dt = http("POST", f"http://127.0.0.1:{port}/session", caps(ctx["browser"]), ctx["session_timeout"])
            rec["svc_pid"] = ctx["prev_svc"] = listener_pid(port)
            rec.update(http_status=st, session_s=round(dt, 3))
            if st == 200:
                sid = json.loads(body)["value"]["sessionId"]
                st2, _, dt2 = http("DELETE", f"http://127.0.0.1:{port}/session/{sid}", timeout=30)
                rec.update(delete_status=st2, delete_s=round(dt2, 3))
            else:
                rec["error"] = body[:400]
                rec["snapshot"] = snapshot(ctx["outdir"], f"{mode}-{rnd}-{i}-session")
        else:
            rec["exited"] = proc.poll() is not None
            rec["rc"] = proc.returncode
            rec["services"] = run(["pgrep", "-fl", "com.apple.WebDriver.HTTPService"]).strip()
            rec["snapshot"] = snapshot(ctx["outdir"], f"{mode}-{rnd}-{i}-nolisten",
                                       [] if rec["exited"] else [proc.pid])
        if mode in ("bare-overlap", "bare-concurrent") and up and proc.poll() is None:
            ctx["held_driver"] = proc
            rec["stop"] = "held for the next iteration"
        else:
            rec["stop"] = stop(proc)
    rec["output"] = open(outpath).read()[:400]
    return rec


def selenium_iteration(ctx, server, mode, rnd, i):
    if mode.endswith("-gap"):
        # Give the previous session's safaridriver time to finish shutting down.
        time.sleep(ctx["gap"])
    rec = {"mode": mode, "round": rnd, "i": i, "t": stamp(), "bidi_8080_busy": listening(8080)}
    result = {}

    def request():
        result["r"] = http("POST", server.base + "/session", caps(ctx["browser"]), ctx["selenium_timeout"])

    th = threading.Thread(target=request, daemon=True)
    th.start()
    t0 = time.monotonic()
    snapped = False
    while th.is_alive():
        th.join(0.5)
        if not snapped and time.monotonic() - t0 > 15:
            snapped = True
            kids = children(server.proc.pid, "safaridriver")
            rec["slow_children"] = run(["ps", "-o", "pid=,etime=,args="] + [x for k in kids for x in ("-p", str(k))]).strip() if kids else ""
            rec["slow_snapshot"] = snapshot(ctx["outdir"], f"{mode}-{rnd}-{i}-slow", kids)
    st, body, dt = result["r"]
    rec.update(http_status=st, session_s=round(dt, 3))
    if st == 200:
        sid = json.loads(body)["value"]["sessionId"]
        st2, _, dt2 = http("DELETE", f"{server.base}/session/{sid}", timeout=60)
        rec.update(delete_status=st2, delete_s=round(dt2, 3))
    else:
        rec["error"] = body[:400]
        kids = children(server.proc.pid, "safaridriver")
        if kids:
            rec["stray"] = run(["ps", "-o", "pid=,etime=,args="] + [x for k in kids for x in ("-p", str(k))]).strip()
            for k in kids:
                stop(k)
    return rec


def summarize(records):
    rows = {}
    for r in records:
        m = rows.setdefault(r["mode"], {"n": 0, "no_listen": 0, "no_listen_exited": 0, "errors": 0,
                                        "over_30s": 0, "times": [], "outputs": set()})
        m["n"] += 1
        if r.get("listened") is False:
            m["no_listen"] += 1
            m["no_listen_exited"] += bool(r.get("exited"))
            if r.get("output"):
                m["outputs"].add(r["output"].strip()[:120])
            continue
        if r.get("http_status") != 200:
            m["errors"] += 1
        if r.get("session_s") is not None:
            m["times"].append(r["session_s"])
            if r["session_s"] > 30:
                m["over_30s"] += 1
    out = {}
    for mode, m in rows.items():
        t = sorted(m["times"])
        out[mode] = {"n": m["n"], "no_listen": m["no_listen"], "no_listen_exited": m["no_listen_exited"],
                     "session_errors": m["errors"], "over_30s": m["over_30s"],
                     "median_s": round(statistics.median(t), 2) if t else None,
                     "max_s": max(t) if t else None, "no_listen_outputs": sorted(m["outputs"])}
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("outdir")
    ap.add_argument("--rounds", type=int, default=10)
    ap.add_argument("--per-round", type=int, default=5)
    ap.add_argument("--modes", default="bare-low,bare-eph,sel3,sel4")
    ap.add_argument("--safaridriver", default="/usr/bin/safaridriver")
    ap.add_argument("--browser", default="safari")
    ap.add_argument("--java", default="java")
    ap.add_argument("--sel3-jar")
    ap.add_argument("--sel4-jar")
    ap.add_argument("--sel4new-jar")
    ap.add_argument("--listen-timeout", type=float, default=20)
    ap.add_argument("--session-timeout", type=float, default=60)
    ap.add_argument("--selenium-timeout", type=float, default=120)
    ap.add_argument("--max-minutes", type=float, help="start no new iteration after this long")
    ap.add_argument("--no-session", action="store_true", help="bare modes only launch and stop safaridriver")
    ap.add_argument("--min-idle", type=float, default=0,
                    help="before each iteration, wait until there has been no user input for this many seconds")
    ap.add_argument("--gap", type=float, default=3, help="pause before each session request in the -gap modes")
    a = ap.parse_args()
    deadline = time.monotonic() + a.max_minutes * 60 if a.max_minutes else None

    os.makedirs(os.path.join(a.outdir, "driver-output"), exist_ok=True)
    modes = a.modes.split(",")
    ctx = {"outdir": a.outdir, "safaridriver": a.safaridriver, "browser": a.browser,
           "listen_timeout": a.listen_timeout, "session_timeout": a.session_timeout,
           "selenium_timeout": a.selenium_timeout, "no_session": a.no_session, "gap": a.gap}

    with open(os.path.join(a.outdir, "environment.txt"), "w") as f:
        f.write(run(["sw_vers"]) + run(["uname", "-m"]) + run([a.safaridriver, "--version"]))
        f.write(run([a.java, "-version"]))
    say(open(os.path.join(a.outdir, "environment.txt")).read().replace("\n", " | "))

    servers = {}
    run_start = datetime.datetime.now()
    jars = {"sel3": (a.sel3_jar, 3), "sel3-gap": (a.sel3_jar, 3), "sel4": (a.sel4_jar, 4), "sel4new": (a.sel4new_jar, 4)}
    records = []
    results = open(os.path.join(a.outdir, "results.jsonl"), "a")
    try:
        for mode in modes:
            if mode in jars:
                jar, major = jars[mode]
                servers[mode] = Selenium(mode, a.java, jar, major, a.safaridriver, a.outdir)
        for rnd in range(1, a.rounds + 1):
            start = datetime.datetime.now()
            order = modes[rnd % len(modes):] + modes[:rnd % len(modes)]
            for mode in order:
                for i in range(1, a.per_round + 1):
                    if deadline and time.monotonic() > deadline:
                        break
                    if a.min_idle:
                        waited = wait_for_idle(a.min_idle)
                        if waited:
                            results.write(json.dumps({"paused_for_input_s": waited, "t": stamp()}) + "\n")
                    if mode in servers:
                        rec = selenium_iteration(ctx, servers[mode], mode, rnd, i)
                    else:
                        rec = bare_iteration(ctx, mode, rnd, i)
                    records.append(rec)
                    results.write(json.dumps(rec) + "\n")
                    results.flush()
                    flag = ""
                    if rec.get("listened") is False:
                        flag = f" NO-LISTEN exited={rec.get('exited')} rc={rec.get('rc')} out={rec.get('output', '')[:80]!r}"
                    elif rec.get("http_status") != 200:
                        flag = f" ERROR {rec.get('http_status')} {rec.get('error', '')[:120]!r}"
                    elif rec.get("session_s", 0) > 30:
                        flag = " SLOW"
                    say(f"r{rnd} {mode} #{i} listen={rec.get('listen_s')} session={rec.get('session_s')}{flag}")
            rinfo = {"round_end": rnd, "t": stamp(), "screen_locked": screen_locked(),
                     "lid_closed": '"AppleClamshellState" = Yes' in run(["ioreg", "-r", "-k", "AppleClamshellState", "-d", "4"]),
                     "interruptions": interruptions_since(start)}
            results.write(json.dumps(rinfo) + "\n")
            results.flush()
            say(f"round {rnd} done: locked={rinfo['screen_locked']} lid_closed={rinfo['lid_closed']} "
                f"interruptions={len(rinfo['interruptions'])}")
            if rinfo["interruptions"] and automation_disabled_since(run_start):
                say("Remote Automation was turned off; stopping")
                break
            if deadline and time.monotonic() > deadline:
                say("time budget used up")
                break
    finally:
        for s in servers.values():
            s.close()
        summary = summarize(records)
        with open(os.path.join(a.outdir, "summary.json"), "w") as f:
            json.dump(summary, f, indent=2)
        for mode, m in summary.items():
            say(f"SUMMARY {mode}: {json.dumps(m)}")


if __name__ == "__main__":
    sys.exit(main())
