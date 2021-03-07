## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## CRAN policies

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> or if those are not available: <https:...>
> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
> auto-linking.
> (If you want to add a title as well please put it in quotes: "Title") 

I have expanded the description field and added a citation, with an ISBN
number. Specific methods used in the package have been cited in the applicable
Rd files. Further acknowledgements and citations are in the README file.

> Please make sure that you do not change the user's options, par or
> working directory. If you really have to do so within functions, please
> ensure with an *immediate* call of on.exit() that the settings are reset
> when the function is exited. e.g.:
> ...
> oldpar <- par(no.readonly = TRUE)    # code line i
> on.exit(par(oldpar))            # code line i + 1
> ...
> par(mfrow=c(2,2))            # somewhere after
> ...
> e.g.: DecisionTree.R
> If you're not familiar with the function, please check ?on.exit. This
> function makes it possible to restore options before exiting a function
> even if the function breaks. Therefore it needs to be called immediately
> after the option change within a function. 

I have now used the `on.exit()` approach described in the comments, ensuring
that the `on.exit` is immediately after reading the default `par` parameters, 
as per instructions. One type of plot created by the package, a "tornado" plot, 
requires adjustment of the plot margin parameters (`mar` and `mai`). These must
be set before the first call to `plot` and I could see no way to avoid using 
a call to `par` altogether in this situation.

> Please do not modify the global environment (e.g. by using <<-) in your
> functions. This is not allowed by the CRAN policies.

All instances of `<<-` have been replaced. There are no other changes to the
global environment.

