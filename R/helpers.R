# p = probability for levs[2] => matrix with probs for levs[1] and levs[2]
prob_vector_to_matrix = function(p, levs) {
  stopifnot(is.numeric(p))
  y = matrix(c(1 - p, p), ncol = 2L, nrow = length(p))
  colnames(y) = levs
  y
}

get_right_levels = function(y) {
  levels_classif = c(unique(y)[1L], unique(y)[2L])
  setDF(levels_classif)
  c(as.character(levels_classif[, 1L]),
    as.character(levels_classif[, 2L]))
}
