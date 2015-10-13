#known issues
- fix loadProfile so that it doesn't cause an overlap with user addExtension calls

#features
- support new w3c webdriver spec: http://www.w3.org/TR/webdriver/
  - try to support backwards compatibility with old wire protocol, otherwise provide legacy API in submodule (?) 
  - mock-up example of actions API: http://lpaste.net/3484676197546196992
- allow WDConfig to automatically load drivers. add modules with driver loading functions
- overload URL inputs/outputs to implicitly support structured URL types
- add support for Opera profiles
- POST "/session/{sessionId}/phantom/execute"

#documentation
- document errors.


#considerations
- consider adding withSession to SessionState so that it can be overloaded easily, or rewriting it so that it's overloaded but not a method itself.
