## Resubmission 

We addressed the following comments from Benjamin Altmann: 
- acronyms are explained in description
- methods reference added in description 
- return value field added where missing

Concerning examples wrapped in dontrun: some examples wrapped in dontrun were 
unwrapped. Some were wrapped in donttest if they are early in the package 
functions pipeline but lengthy to run. Some were left as dontrun for either of 
two reasons: 1. some are internal functions which are not meant to be ran by 
users, 2. most are due to the fact that this is a workflow-based package, and 
each step requires all previous steps to be ran: this creates very large, very 
heavy and hard to understand examples. Instead, we have written lots of tests 
and have a vignette that explain the whole workflow. I'd like to add that many 
users interpret the "not run" comment more as "has not been run" instead of "do 
not run". In our workflow based package, this should make sense to leave a lot 
of the examples wrapped in dontrun, with the tests to provide coverage and 
vignettes to show how to run the pipeline.
