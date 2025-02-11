## Resubmission 

We addressed the following comments from Benjamin Altmann: 
- acronyms are explained in description
- methods reference added in description 
- return value field added where missing

Concerning examples wrapped in dontrun: some examples wrapped in dontrun were 
unwrapped. Some were left as dontrun for either of two reasons: 
  
  1. Some are internal functions which are not meant to be ran by users
  
  2. Most are due to the fact that this is a workflow-based package, in which 
  each step build upon the previous one. Each step requires all previous steps 
  to be run. This lead to examples that reproduce the entire workflow each time, 
  and therefore even if wrapped in donttest, are very lengthy and costly when 
  sporadically ran by CRAN. Even without those examples being run, we have a 
  very high test coverage percentage.The vignette provides education as to how 
  to run the workflow, and the examples are more there to illustrate usage. I'd 
  like to add that many users interpret the "# Not run:" comment more as "has 
  not been run" instead of "do not run". Therefore in our case, it should make 
  sense to leave a lot of the examples wrapped in dontrun, with the tests and 
  vignettes to complete the picture of usage.
