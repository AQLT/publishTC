## @import rjd3filters
#' @import rjd3x13
#' @importFrom rJava .jcall .jaddClassPath

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
