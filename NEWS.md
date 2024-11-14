# tsmethods 1.0.2

* Added a distribution_list function to validate and return
an object of class `tsmodel.distribution_list` which holds a
validated list of `tsmodel.distribution` objects for use in
multivariate models.
* Added method to convert tsmodel.distribution and tsmodel.distribution_list
objects to a long form data table for easier export to databases.
* Added methods for tsconvolve, tscoskew and tscokurt which are used
in the upcoming tsmarch package independent factor conditional density model.

# tsmethods 1.0.1

* Added a `NEWS.md` file to track changes to the package.
* Removed Changelog.
* Fixed the as.Date error in the tests for the oldrel version checks.

# tsmethods 1.0.0

* Release to CRAN

# tsmethods 0.3.2

* Added additional methods of halflife, unconditional and pit for use with
tsgarch and tsarma.

# tsmethods 0.3.1

* Converted documentation to Roxygen and performed some clean up.

# tsmethods 0.3.0

* Added a tsmoments method
* Bumped the version to 0.3.0 to be consistent with major changes in other 
packages with the same version.

# tsmethods 0.1.8

* Added a tscalibrate method

# tsmethods 0.1.7

* Added additional estimate_ad method for new plugin packages which use
automatic differentiation.
* Fix to tsgrowth documentation

# tsmethods 0.1.6

* Fix to prediction plots to allow missing values in observed series (ylim 
fixed for na.rm)

# tsmethods 0.1.5

* Fix to tsreport method (previously dispatched to report rather than tsreport).
* Added Changelog to package

