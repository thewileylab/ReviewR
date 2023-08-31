
## Update Summary

This is a minor update to the ReviewR package, bringing the version to  2.3.9

Fixes

*  Address Rd issue with package overview help file

Misc Updates

* minor version bump
* Remove `dashboardthemes` dependency
* Refresh vignettes with updated URLs
* update dockerfile(s)

## Test environments

### GitHub Actions

* windows-latest (release), R 4.3.1
* macOS-latest (release), R 4.3.1
* ubuntu-22.04.3 (release), R 4.3.1
* ubuntu-22.04.3 (old-release), R 4.2.3
* ubuntu-22.04.3 (devel), R devel

### R-hub

* Windows Server 2022, R-devel, 64 bit
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

### Win-builder

* R-devel

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking package dependencies ... NOTE
  Imports includes 28 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable. Move as many as possible to Suggests and
  use conditionally.
  
This package utilizes the R Tidyvrese, which has a defined [lifecycle](https://lifecycle.r-lib.org/articles/stages.html) for all functions. This will allow ample time to locate and update any deprecated/retired functions before they become unavailable. 
