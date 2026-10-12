// Hand-maintained overrides. Firefox reads this after prefs.js, so these win.
user_pref("browser.startup.homepage", "https://example.com/");
user_pref("network.trr.mode", 2);

# Firefox's parser also accepts shell-style line comments...
user_pref("ui.systemUsesDarkTheme", 1);

/* ...and C-style block
   comments. */
user_pref("signon.rememberSignons", true);
