
## Update Summary

This is a minor update to the ReviewR package, bringing the version to  2.3.7

Fixes
  * Addresses an issue with a development function, intended to help developers extend the functionality of ReviewR.
  * The demonstration database has been updated.

## Test environments

### GitHub Actions

* windows-latest (release), R 4.0.4 
* macOS-latest (release), R 4.0.4
* ubuntu-20.04 (release), R 4.0.4
* ubuntu-20.04 (devel), R devel

### R-hub

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * libcurl error code 35 returned for some vignettes URLs - I believe this to be an error as identified URLs are still active and pass the same checks on other platforms.
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * PREPERROR, though overall r-hub build process appears to have succeeded. All R CMD checks appear to pass.
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
