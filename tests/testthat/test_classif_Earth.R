library(earth)
library(MASS)

context("classif.earth")

# autotest
test_that("autotest", {
  learner = LearnerClassifEarth$new()
  learner$param_set$values = list(degree = 2L)
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

# test on real datasets
test_that("Training works.", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  Boston$chas <- as.factor(Boston$chas)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "chas")
  learner = LearnerClassifEarth$new()
  learner$param_set$values = list(degree = 2L)
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  learner$train(task, row_ids = train_set)
})

test_that("Prediction works (response).", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  Boston$chas <- as.factor(Boston$chas)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "chas")
  learner = LearnerClassifEarth$new()
  learner$param_set$values = list(degree = 2L)
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
})

test_that("Prediction works (prob).", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  Boston$chas <- as.factor(Boston$chas)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "chas")
  learner = LearnerClassifEarth$new()
  learner$param_set$values = list(degree = 2L)
  learner$predict_type <- "prob"
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
})

# comparison to package
test_that("Learner produces similar / identical results as package version (classes: '0', '1', type: 'response')", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  Boston$chas <- as.factor(Boston$chas)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "chas")
  learner = LearnerClassifEarth$new()
  learner$param_set$values = list(degree = 2L)
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)

  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)

  model = earth(x = Boston[, task$feature_names], y = Boston$chas,
                subset = train_set, degree = 2L,
                glm = list(family = "binomial"))
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "class")

  expect_true(all.equal(prediction$response, as.factor(as.vector(unname(pred)))))
})

test_that("Learner produces similar / identical results as package version (classes: '0', '1', type: 'prob')", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  Boston$chas <- as.factor(Boston$chas)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "chas")
  learner = LearnerClassifEarth$new()
  learner$param_set$values = list(degree = 2L)
  learner$predict_type <- "prob"
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)

  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)

  model = earth(x = Boston[, task$feature_names], y = Boston$chas,
                subset = train_set, degree = 2L,
                glm = list(family = "binomial"))
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "response")

  expect_true(all.equal(prediction$prob[, 2], as.vector(unname(pred))))
})

test_that("Learner produces identical results as package version (classes: 'Yes', 'No', type: 'response')", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  Boston$crim_fac <- "No"
  Boston$crim_fac[Boston$crim > median(Boston$crim)] <- "Yes"
  Boston$crim_fac <- factor(Boston$crim_fac)
  Boston$crim_fac[c(1, 5, 176)] <- "Yes" # otherwise problem is perfectly linearly separable which results in warning.
  Boston <- Boston[ , "crim" != colnames(Boston)]
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "crim_fac")
  learner = LearnerClassifEarth$new()
  learner$param_set$values = list(degree = 2L)
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)

  model = earth(x = Boston[, task$feature_names], y = Boston$crim_fac,
                subset = train_set, degree = 2L,
                glm = list(family = "binomial"))
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "class")

  expect_true(all.equal(prediction$response, as.factor(as.vector(unname(pred)))))
})

test_that("Learner produces identical results as package version (classes: 'Yes', 'No', type: 'prob')", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  Boston$crim_fac <- "No"
  Boston$crim_fac[Boston$crim > median(Boston$crim)] <- "Yes"
  Boston$crim_fac <- factor(Boston$crim_fac)
  Boston$crim_fac[c(1, 5, 176)] <- "Yes" # otherwise problem is perfectly linearly separable which results in warning.
  Boston <- Boston[ , "crim" != colnames(Boston)]
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "crim_fac")
  learner = LearnerClassifEarth$new()
  learner$param_set$values = list(degree = 2L)
  learner$predict_type <- "prob"
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)

  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)

  model = earth(x = Boston[, task$feature_names], y = Boston$crim_fac,
                subset = train_set, degree = 2L,
                glm = list(family = "binomial"))
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "response")

  expect_true(all.equal(prediction$prob[, 2], as.vector(unname(pred))))
})

# parameter acceptance
test_that("Learner accepts all meaningful inputs and proceeds with training and prediction and results are identical to package.", {
  set.seed(20191022)
  data(Boston, package = "MASS")
  Boston$chas <- as.factor(Boston$chas)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "chas")
  learner = LearnerClassifEarth$new()
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)

  weights = sample(c(1L, 2L), length(train_set), replace = TRUE)
  wp = c(1L, 2L)
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
                y = Boston$chas[train_set], weights = weights,
                degree = degree, penalty = penalty, nk = nk, thresh = thresh,
                minspan = minspan, endspan = endspan,
                newvar.penalty = newvar_penalty, fast.k = fast_k,
                fast.beta = fast_beta, linpreds = linpreds, pmethod = pmethod,
                nprune = nprune)
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "class")
  expect_true(all.equal(as.character(prediction$response), as.vector(unname(pred))))
})

