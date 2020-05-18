rm(list=ls())

local({
  source('rdecision-dev.R')

  mv <- ModelVariable$new('Variable description', 'GBP')
  v.env <- mv$get_environment()
  rlang::env_print(v.env)
  c.env <- rlang::current_env()
  rlang::env_print(c.env)
  #print(rlang::env_parents(c.env))
  
})