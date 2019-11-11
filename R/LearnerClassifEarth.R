#' @title MARS (Multivariate Adaptive Regression Splines) Classification
#' (binary)
#'
#' @aliases mlr_learners_classif.earth
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#'
#' @section Construction:
#' ```
#' LearnerClassifRanger$new()
#' mlr3::mlr_learners$get("classif.earth")
#' mlr3::lrn("classif.earth")
#' ```
#'
#' @description
#' A [LearnerClassif] for MARS implemented in earth::earth()] in package
#' \CRANpkg{earth}.
#' This package is derived from mda::mars()] in package \CRANpkg{mda}.
#' The classification framework is derived via the the arguments of earth.
#' In particular we set the \code{glm} argument to
#' \code{list(family = "binomial")}
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
LearnerClassifEarth = R6Class("LearnerClassifEarth",
  inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.earth") {
      super$initialize(
        id = id, packages = "earth", feature_types = c("numeric", "factor", "integer"),
        predict_types = c("response", "prob"), param_set = ParamSet$new(
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
        properties = c("weights", "twoclass")
      )
    },
    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()
      f_names = task$feature_names
      t_names = task$target_names
      x = data[, ..f_names]
      y = data[, ..t_names]
      model = invoke(earth::earth, x = x, y = y, glm = list(family = binomial), .args = pars)
      model$levels = get_right_levels(y)
      model
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      # glm naming vs. mlr3
      p = unname(predict(self$model, newdata = newdata, type = "response"))
      levs = self$model$levels
      if (self$predict_type == "response") {
        pred = PredictionClassif$new(task = task, response = ifelse(p < 0.5, levs[1L], levs[2L]))
      } else {
        pred = PredictionClassif$new(task = task, prob = prob_vector_to_matrix(p, levs))
      }
    }
  )
)
