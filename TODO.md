# Fix to reach "beta" quality

- Simulation of reductions stops before reaching a fixpoint (urgent)
  - DONE: Fixed in interpreter
  - TODO: Port the fix to the compiler
- Rule ranking is too simplistic (urgent)
- Capture is not always correct
- Reduction does not commute with concatenation 
  (easy to fix, lots of code to change)
  
=> These limitations are more-or-less all due to the compilation scheme,
   I need to rewrite it completely

# Provide an lrgrep-enabled Merlin branch 

WIP

# Workflow documentation

Missing parts: 
- Walkthrough starting from a syntax error to a new rule
- Description of the pattern language and rule file syntax 
- The preprocessor is there (`-pp ocaml/frontend.exe`),
  document deployment workflow starting from a rule change to a new frontend
