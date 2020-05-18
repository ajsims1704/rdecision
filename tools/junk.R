rm(list=ls())
source('rdecision-dev.R')

local({

  mv <- ModelVariable$new('Variable description', 'GBP')
  p.env <- parent.env(rlang::current_env())
  rlang::env_print(p.env)
  #print(as.list.environment(mv))
  v.env <- mv$get_environment()
  #rlang::env_print(v.env)
  c.env <- rlang::current_env()
  #rlang::env_print(c.env)
  #print(rlang::env_parents(c.env))
  
  x <- rlang::env_names(rlang::global_env())
  print(x)
  
})