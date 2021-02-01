## Resubmission
This is a resubmission. In this version I have:

* Updated vignettes so that example URLs are escaped as code chunks, rather than as hyperlinks to nonexistent web locations. These example URLs will guide users if deploying the ReviewR package on a Shiny Server or with Docker and will be specific to individual user's production environment. Example URLs will not impact users that use the ReviewR package locally. 

* Further reduced the file size of the vignette documentation by reducing png size. 

* Incremented the package version number to v2.3.2. 

## Test environments
All tests were performed using GitHub Actions

* windows-latest (release), R 4.0.3 
* macOS-latest (release), R 4.0.3 
* ubuntu-20.04 (release), R 4.0.3 
* ubuntu-20.04 (devel), R devel

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking package dependencies ... NOTE
  Imports includes 30 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
  
  This package utilizes the R Tidyvrese, which has a defined [lifecycle](https://lifecycle.r-lib.org/articles/stages.html) for all functions. This will allow ample time to locate and update any deprecated/retired functions before they become unavailable. 
