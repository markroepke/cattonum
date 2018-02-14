####################
### hash_labeler ###
####################

# https://github.com/eddelbuettel/digest/issues/63

hash_labeler <- function(.x, .ncol, .seed) {
  unique_levels <- as.character(stats::na.omit(unique(.x)))
  hashes <- vapply(unique_levels,
                   digest::digest,
                   character(1),
                   algo = "murmur32",
                   serialize = FALSE,
                   seed = .seed)
  numeric_hashes <- as.numeric(Rmpfr::mpfr(hashes, base = 16))
  data.frame(new_lab = numeric_hashes %% .ncol,
             row.names = unique_levels)
}

####################
### hash_to_fact ###
####################

hash_to_fact <- function(.x, .lkp, .ncol, .colname) {
  x_hash <- encode_from_lkp(x, .lkp)
  x_hash_char <- paste0(.colname, hashed_x)
  hash_fact_levels <- paste(.colname, seq_len(.ncol) - 1, sep = "_")
  factor(x_hash_char, levels = hash_fact_levels) # model.matrix
}

##################
### catto_hash ###
##################

#' Feature hashing
#'
#' @param train The training data, in a \code{data.frame} or \code{tibble}.
#' @param ... The columns to be encoded.  If none are specified, then
#'   all character and factor columns are encoded.
#' @param test The test data, in a \code{data.frame} or \code{tibble}.
#' @param num_cols The response variable used to calculate means.
#' @param seed The seed to use for the MurmurHash algorithm
#' @param verbose Should informative messages be printed?  Defaults to
#'   \code{TRUE}.
#' @return The encoded dataset in a \code{data.frame} or \code{tibble},
#'   whichever was input.  If a test dataset was provided, a named list
#'   is returned holding the encoded training and test datasets.
#' @examples
#' catto_hash(iris)
#' @export
catto_hash <- function(train,
                       ...,
                       test,
                       num_cols = 2 ^ 10,
                       seed = 4444,
                       verbose = TRUE) {

  validate_col_types(train)
  test_also <- ! missing(test)
  if (test_also) check_train_test(train, test)

  nms <- names(train)

  cats <- pick_cols(train, ...)

  hash_lkps <- lapply(train[cats],
                      hash_labeler,
                      .ncol = num_cols,
                      .seed = seed)

  if (! test_also) {
    train
  } else {
    test[cats] <- encode_from_lkp(test[cats], mean_lkps)
    list(train = train, test = test)
  }

}

###