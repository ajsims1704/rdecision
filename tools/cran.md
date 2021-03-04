
# rdecision 1.0.2

* `devtools:check_rhub()` failed on the windows development servers with a 
  namespace
  error. This turned out to be because the package `utf8` is now required by
  the development version of package `testthat`. However `utf8` is not
  available as a windows binary for the latest development release of R.
  To fix, by forcing the package to be compiled from source, use:
  
```r
     devtools::check_hub(
       platform = "windows-x86_64-devel",
       env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES="always")
     )
```

* At least two references include a DOI which includes a forward slash (/)
  character. This triggers a HTTP not found error during checks, for
  Rd files created with the `\doi{}` macro, and in vignettes with a DOI
  link created by the csl file. To do: investigate how to escape illegal
  characters in DOIs.
  
  
