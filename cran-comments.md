
## Resubmission 4

This is the fourth resubmission of the 'ReviewR' package. In this version I have addressed the following identified issues:

* Documentation overhaul
  * new shiny module documentation format, which combines shiny module ui and server functions into a single Rd
  * Improved return documentation for shiny modules, including the contents of reactiveValues objects
  * module functions are no longer exported, as they are not intended to be used individually
* Re-evaluate which functions need to be user accessible via @export
* Add missing Rd-tags for exported package functions
* Misc
  * update to latest shinydashboardPlus and shinyWidgets packages

## Test environments

### GitHub Actions

* windows-latest (release), R 4.0.4 
* macOS-latest (release), R 4.0.4
* ubuntu-20.04 (release), R 4.0.4
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
