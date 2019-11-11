#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

register_mlr3 = function() {
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  x$add("regr.earth", LearnerRegrEarth)
  x$add("classif.earth", LearnerClassifEarth)
}

.onLoad = function(libname, pkgname) {
  # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
} # nocov end
