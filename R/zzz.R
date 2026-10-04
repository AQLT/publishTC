## @import rjd3filters
#' @importFrom rjd3x13 regarima_outliers
#' @importFrom rJava .jcall .jaddClassPath
NULL

#' @noRd
#' @keywords internal
#' @importFrom rjd3jars check_java_version
.onLoad <- function(libname, pkgname) {
	# if (!requireNamespace("rjd3filters", quietly = TRUE)) stop("Loading rjd3 libraries failed")
	# if (!requireNamespace("rjd3x13", quietly = TRUE)) stop("Loading rjd3 libraries failed")

	# result <- rJava::.jpackage(pkgname, lib.loc=libname)

	path_jar <- system.file("java", package = "rjd3x13")
	if (file.exists(path_jar)) {
		.jaddClassPath(path_jar)
	} else {
		stop("Install rjd3x13 package")
	}
	# reload extractors
	# rjd3toolkit::reload_dictionaries()
}
