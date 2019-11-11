#' @title MARS (Multivariate Adaptive Regression Splines)
#'
#' @aliases mlr_learners_regr.earth
#' @format [R6::R6Class] inheriting from [LearnerRegr].
#'
#' @section Construction:
#' ```
#' LearnerRegrEarth$new()
#' mlr3::mlr_learners$get("regr.earth")
#' mlr3::lrn("regr.earth")
#'
#' @description
#' A [LearnerRegr] for MARS implemented in earth::earth()] in package
#' \CRANpkg{earth}.
#' This package is derived from mda::mars()] in package \CRANpkg{mda}.
#' Methods for variance estimations have not been implemented so far.
#'
#' @references
#' Stephen Milborrow (2014)
#' Earth: multivariate adaptive regression spline models
#' R package version 3
#' \url{https://cran.r-project.org/web/packages/earth/earth.pdf}
#'
#' Jerome H. Friedman (1991)
#' Multivariate Adaptive Regression Splines
#' The Annals of Statistics
#' \url{https://projecteuclid.org/download/pdf_1/euclid.aos/1176347963}
#'
#' @export
LearnerRegrEarth = R6Class("LearnerRegrEarth", inherit = LearnerRegr, public = list(
  initialize = function(id = "regr.earth") {
    super$initialize(
      id = id, packages = "earth",
      feature_types = c("numeric", "factor", "integer"),
      predict_types = c("response"),
      param_set = ParamSet$new(
        params = list(
          ParamUty$new(id = "weights", default = NULL, tags = "train"),
          ParamInt$new(id = "degree", default = 1L, tags = "train"),
          ParamDbl$new(id = "penalty", default = 2L, lower = -1L, tags = "train"),
          ParamUty$new(id = "nk", default = NULL, tags = "train"),
          ParamDbl$new(id = "thresh", default = 0.001, tags = "train"),
          ParamDbl$new(id = "minspan", default = 0L, tags = "train"),
          ParamDbl$new(id = "endspan", default = 0L, tags = "train"),
          ParamDbl$new(id = "newvar.penalty", default = 0L, tags = "train"),
          ParamInt$new(id = "fast.k", default = 20L, tags = "train"),
          ParamInt$new(id = "fast.beta", lower = 0L, upper = 1L, default = 1L, tags = "train"),
          ParamUty$new(id = "linpreds", default = FALSE, tags = "train"),
          ParamUty$new(id = "pmethod", default = "backward", tags = "train"),
          ParamUty$new(id = "nprune", default = NULL, tags = "train")
        )
      ),
      properties = c("weights")
    )
  },
  train_internal = function(task) {
    pars = self$param_set$get_values(tags = "train")
    data = task$data()
    f_names = task$feature_names
    t_names = task$target_names
    x = data[, ..f_names]
    y = data[, ..t_names]
    invoke(earth::earth, x = x, y = y, .args = pars)
  },

  predict_internal = function(task) {
    pars = self$param_set$get_values(tags = "predict")
    # as of now not necessary.
    newdata = task$data(cols = task$feature_names)
    p = invoke(predict, self$model, newdata = newdata, type = "response", .args = pars)
    PredictionRegr$new(task = task, response = p)
  }
)
)
