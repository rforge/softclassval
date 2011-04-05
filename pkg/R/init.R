.onLoad <- function (libname, pkgname){
#  stopifnot (require (arrayhelpers))
}

.onAttach <- function (libname, pkgname){
  ## unlockBinding(".options", asNamespace("softclassval")) 

  desc <- utils::packageDescription("softclassval")
  vers <- paste("V. ", desc$Version)

  cat ("Package ",  desc$Package, ", version ", desc$Version, "\n\n",
       "If you use this package please cite it appropriately.\n",
       "   citation(\"softperf\")\nwill give you the correct reference.", "\n\n",
       "The project homepage is http://softclassval.r-forge.r-project.org\n\n",
       sep = "")
}

