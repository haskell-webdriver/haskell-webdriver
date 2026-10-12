# Test fixtures

Hand-written, not copied from a real profile.

- `firefox-profile/prefs.js`: the preamble is `kPrefFileHeader` from Firefox's
  `modules/libpref/Preferences.cpp`, the format since Firefox 60. The entries use preference
  names that appear verbatim in the Firefox source, with made-up values.
- `prefs-firefox-59.js`: the same, with the preamble and preference names from the Firefox
  59.0 release source, the format before Firefox 60.
- `firefox-profile/user.js`: exercises the three comment styles Firefox's prefs parser accepts.
- The other files in `firefox-profile/` only need to exist and be read back; their contents
  are illustrative.
