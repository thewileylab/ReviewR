
## Resubmission 3

This is a third resubmission of the 'ReviewR' package. In this version I have addressed the following identified issues:

* Add protocol to the for the contributing code of conduct in README.md
* Add organization to ORGANIZATION field of LICENSE file, per CRAN template
* Incremented the package version number to v2.3.4

## Test environments

### GitHub Actions

* windows-latest (release), R 4.0.3 
* macOS-latest (release), R 4.0.3 
* ubuntu-20.04 (release), R 4.0.3 
* ubuntu-20.04 (devel), R devel

### R-hub

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

### Win-builder

* R-devel

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking package dependencies ... NOTE
  Imports includes 30 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
  
  This package utilizes the R Tidyvrese, which has a defined [lifecycle](https://lifecycle.r-lib.org/articles/stages.html) for all functions. This will allow ample time to locate and update any deprecated/retired functions before they become unavailable. 

---

## Resubmission 2

This is a second resubmission of the 'ReviewR' package. In this version I have addressed the following identified issues:

* Provided a fully specified URL for CODE_OF_CONDUCT.md within the README.md file

* Updated the DESCRIPTION file, utilizing single quotes for non English usage / software names. 

* Incremented the package version number to v2.3.3

## Test environments

### GitHub Actions

* windows-latest (release), R 4.0.3 
* macOS-latest (release), R 4.0.3 
* ubuntu-20.04 (release), R 4.0.3 
* ubuntu-20.04 (devel), R devel

### R-hub

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

### Win-builder

* R-devel

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking package dependencies ... NOTE
  Imports includes 30 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
  
  This package utilizes the R Tidyvrese, which has a defined [lifecycle](https://lifecycle.r-lib.org/articles/stages.html) for all functions. This will allow ample time to locate and update any deprecated/retired functions before they become unavailable. 

---

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
