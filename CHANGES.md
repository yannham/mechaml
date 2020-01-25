1.2.0
-----
 - Implement OCaml custom binding operators (let+, and+, let*, and*) for the
     Agent monad

1.1.0
-----
 - Add a Format module to help generating formatted contents for forms
 - Migration from Oasis to Dune
 - Compatibility with OCaml 4.07

1.0.0
-----
 - Breaking change: separate the agent data and server's response in the Agent module
 - add an Agent.Monad module that combines state (agent) and promises (Lwt) together with associated helpers
 - add wrappers and support for Lambdasoup lazy traversal of HTML documents
 - handle default values and hidden inputs in forms
 - handle multiple select lists
 - remove code of unspported features (file upload, proxy, ...)
 - improve tests and documentation
 - switch to 3-digits versionning
