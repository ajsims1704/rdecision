## Test environments
* local R installation, R 4.0.4
* ubuntu 16.04 (on travis-ci), R 4.0.4
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

There was 1 NOTE:

Namespaces in Imports field not imported from:
  ‘R6’ ‘grid’ ‘rlang’ ‘stats’ ‘utils’
   All declared Imports should be used. 

* R6 is a build-time dependency.
  
## Random test failures

> Dear maintainer,
> 
> Please see the problems shown on
> <https://eur03.safelinks.protection.outlook.com/?url=https%3A%2F%2Fcran.r-project.org%2Fweb%2Fchecks%2Fcheck_results_rdecision.html&amp;data=04%7C01%7Candrew.sims%40newcastle.ac.uk%7C196c828d42c8415cae5008d8e5f19dde%7C9c5012c9b61644c2a91766814fbe3e87%7C1%7C0%7C637512173473338530%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C2000&amp;sdata=caH4ZlxfYb%2Bb1x6fRql6vil%2Biz%2BEftoNFeCXsAQ4bU8%3D&amp;reserved=0>.
> 
> Please correct before 2021-03-27 to safely retain your package on CRAN.
>
> The CRAN Team

Package tests that involve sampling randomly from a distribution and
comparing the results with parameters of an expected distribution have been 
excluded when running CRAN tests because they fail at a type I error rate
of about 0.1%.
  
  
