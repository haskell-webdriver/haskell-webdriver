#known issue
- fix loadProfile so that it doesn't cause an overlap with user addExtension calls

#features
- add support for Opera profiles
- improve documentation
- 
#documentation
- document errors.
- provide examples
  - basic runSession usage
  - intermediate usage
  - exception handling with lifted-base
  - explicit waits usage
  - firefox profile usage
  - REPL usage
- allow WDConfig to automatically load drivers. add modules with driver loading functions


#considerations
- consider adding withSession to SessionState so that it can be overloaded easily, or rewriting it so that it's overloaded but not a method itself.
