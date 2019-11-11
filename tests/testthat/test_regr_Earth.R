library(earth)
library(MASS)
context("regr.earth")

# autotest
test_that("autotest", {
  learner = LearnerRegrEarth$new()
  learner$param_set$values = list(degree = 2L)
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

# test on real datasets
test_that("Training works.", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  task = TaskRegr$new(id = "Boston", backend = Boston, target = "crim")
  learner = LearnerRegrEarth$new()
  learner$param_set$values = list(degree = 2L)
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  learner$train(task, row_ids = train_set)
})

test_that("Prediction works.", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  task = TaskRegr$new(id = "Boston", backend = Boston, target = "crim")
  learner = LearnerRegrEarth$new()
  learner$param_set$values = list(degree = 2L)
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
})

# comparison to package
test_that("Learner produces identical results as package version.", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  task = TaskRegr$new(id = "Boston", backend = Boston, target = "crim")
  learner = LearnerRegrEarth$new()
  learner$param_set$values = list(degree = 2L)
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)

  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)

  model = earth(x = Boston[, task$feature_names], y = Boston$crim, subset = train_set, degree = 2L)
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "response")

  expect_true(all.equal(prediction$response, as.vector(unname(pred))))
})

# parameter acceptance
test_that("Learner accepts all meaningful inputs and proceeds with training and prediction and results are identical to package.", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  task = TaskRegr$new(id = "Boston", backend = Boston, target = "crim")
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  learner = LearnerRegrEarth$new()

  weights = sample(c(1L, 2L), length(train_set), replace = TRUE)
  degree = 2L
  penalty = 4L
  nk = 2L
  thresh = 0.01
  minspan = 1L
  endspan = 1L
  newvar_penalty = 0.02
  fast_k = 5L
  fast_beta = 0L
  linpreds = c(TRUE, TRUE, rep(FALSE, 11L))
  pmethod = "forward"
  nprune = 5L

  learner$param_set$values = list(weights = weights,
    degree = degree,
    penalty = penalty,
    nk = nk,
    thresh = thresh,
    minspan = minspan,
    endspan = endspan,
    newvar.penalty = newvar_penalty,
    fast.k = fast_k,
    fast.beta = fast_beta,
    linpreds = linpreds,
    pmethod = pmethod,
    nprune = nprune)

  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)

  model = earth(x = Boston[train_set, task$feature_names],
    y = Boston$crim[train_set], weights = weights,
    degree = degree, penalty = penalty, nk = nk, thresh = thresh,
    minspan = minspan, endspan = endspan,
    newvar.penalty = newvar_penalty, fast.k = fast_k,
    fast.beta = fast_beta, linpreds = linpreds, pmethod = pmethod,
    nprune = nprune)
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "response")

  expect_true(all.equal(prediction$response, as.vector(unname(pred))))
})
