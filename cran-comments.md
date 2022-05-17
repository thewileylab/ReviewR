
## Update Summary

This is a minor update to the ReviewR package, bringing the version to  2.3.8

Fixes

* Address Rd issue in `mod_layout_chartreview()`
  
    >In Rd \describe, \arguments and \value the item entries are of the form
    >
    >\item{LABEL}{DESCRIPTION}
    >
    >with a non-empty label: please modify your Rd files accordingly.

Misc Updates

* minor version bump
* move pkgload to suggests
* use latest roxygen
* update dockerfile
* add package CITATION

## Test environments

### GitHub Actions

* windows-latest (release), R 4.2.0
* macOS-latest (release), R 4.2.0
* ubuntu-20.04 (release), R 4.2.0
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
  Imports includes 29 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
  
This package utilizes the R Tidyvrese, which has a defined [lifecycle](https://lifecycle.r-lib.org/articles/stages.html) for all functions. This will allow ample time to locate and update any deprecated/retired functions before they become unavailable. 
