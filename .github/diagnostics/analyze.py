#!/usr/bin/env python3
# Combine results.jsonl files from safari-launch-stress.py runs into one table.
# Usage: analyze.py LABEL=DIR [LABEL=DIR ...]   (DIR holds results.jsonl and snapshots/)
import json
import os
import re
import statistics
import sys


def classify(r):
    if r.get("listened") is False:
        return "no-listen(exited)" if r.get("exited") else "no-listen(hung)"
    if r.get("listened") and "http_status" not in r:
        return "ok"
    st = r.get("http_status")
    err = r.get("error", "") or ""
    if st == 200:
        return "slow-ok" if r.get("session_s", 0) > 30 else "ok"
    if st is None:
        return "client-timeout"
    if "already paired" in err:
        return "already-paired"
    if "timed out while connecting" in err:
        return "safari-connect-timeout"
    if "Failed to connect" in err or "Connection refused" in err or "ConnectException" in err:
        return "driver-unreachable"
    return "other-error"


def selenium_child_listening(r):
    """For a slow Selenium request, was the safaridriver it launched listening 15 s in?"""
    path = r.get("slow_snapshot")
    kids = r.get("slow_children", "")
    if not path or not kids:
        return None
    ports = re.findall(r"--port (\d+)", kids)
    if not os.path.exists(path):
        return None
    text = open(path).read()
    listeners = text.split("== listeners", 1)[-1].split("== ", 1)[0]
    return {p: (f":{p} (LISTEN)" in listeners) for p in ports}


def main():
    rows = []
    for arg in sys.argv[1:]:
        label, d = arg.split("=", 1)
        path = os.path.join(d, "results.jsonl")
        if not os.path.exists(path):
            print(f"{label}: no results.jsonl in {d}")
            continue
        recs = [json.loads(l) for l in open(path) if l.strip()]
        rounds = [r for r in recs if "round_end" in r]
        its = [r for r in recs if "mode" in r]
        interrupted = sum(len(r.get("interruptions", [])) for r in rounds)
        locked = sum(1 for r in rounds if r.get("screen_locked"))
        env = open(os.path.join(d, "environment.txt")).read() if os.path.exists(os.path.join(d, "environment.txt")) else ""
        ver = " ".join(re.findall(r"ProductVersion:\s*(\S+)", env) + re.findall(r"Included with (Safari[^(]*)", env)).strip()
        print(f"\n== {label}: {ver}; {len(its)} iterations, {len(rounds)} rounds, "
              f"interruption log lines {interrupted}, locked rounds {locked}")
        modes = []
        for r in its:
            if r["mode"] not in modes:
                modes.append(r["mode"])
        for mode in modes:
            rs = [r for r in its if r["mode"] == mode]
            kinds = {}
            for r in rs:
                k = classify(r)
                kinds[k] = kinds.get(k, 0) + 1
            times = sorted(r["session_s"] for r in rs if r.get("http_status") == 200)
            listen = sorted(r["listen_s"] for r in rs if r.get("listened"))
            busy = sum(1 for r in rs if r.get("bidi_8080_busy"))
            prev_alive = sum(1 for r in rs if r.get("prev_svc_alive"))
            line = (f"  {mode:8} n={len(rs):3} " + " ".join(f"{k}={v}" for k, v in sorted(kinds.items()))
                    + (f" | session median {statistics.median(times):.2f}s max {max(times):.2f}s" if times else "")
                    + (f" | listen median {statistics.median(listen):.2f}s max {max(listen):.2f}s" if listen else "")
                    + f" | 8080 busy at start {busy}" + (f" | prev helper alive {prev_alive}" if mode.startswith("bare") else ""))
            print(line)
            for r in rs:
                k = classify(r)
                if k not in ("ok",):
                    extra = ""
                    if mode.startswith("sel"):
                        extra = f" child-listening@15s={selenium_child_listening(r)}"
                    print(f"      r{r['round']}#{r['i']} {k} session_s={r.get('session_s')} listen_s={r.get('listen_s')}"
                          f" 8080busy={r.get('bidi_8080_busy')} prevsvc={r.get('prev_svc_alive')}{extra}"
                          f" err={(r.get('error') or r.get('output') or '')[:140]!r}")
        rows.append(label)


if __name__ == "__main__":
    main()
