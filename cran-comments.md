
## Resubmission 5

This is the fifth resubmission of the 'ReviewR' package. In this version I have addressed the following identified issues:

* Documentation: 
  * Remove @examples for unexported functions
* Functions
  * Remove `dev_add_google_client_id` function which assisted with placing a Google Client ID JSON file in a location accessible by ReviewR. Instead, allow a user to specify a path to a Google ClientID *already located* on their system. The BigQuery module will still check a few locations depending on the detected platform but users are no longer required to use these locations and may specify their own. None of this is necessary if users wish to use the Google ClientID built into this package. 
* Misc
  * Update version to 2.3.6

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
