#' Compute most similar words to a dictionary of seed words
#'
#' @param dictionary a character vector of words containing wildcards
#' @param vectors A vectors object, e.g. as returned by load_fasttext
#' @param k the (maximum) number of neighbours to return
#' @return along-format tibble containing columns word and dist
#' @export
similar_words = function(dictionary, vectors, antonyms=NULL) {
  centroid <- get_centroid(dictionary, vectors)
  similarities <- (vectors$vectors %*% centroid)[,1]
  similarities <- tibble::tibble(word=names(similarities), similarity=similarities) |>
    dplyr::arrange(-similarity) |>
    dplyr::filter(!word %in% dictionary)

  if (!is.null(antonyms)) {
    antisim <- similar_words(antonyms, vectors)
    similarities <- dplyr::left_join(similarities, antisim, by="word", suffix=c("", ".anti")) |>
      dplyr::filter(is.na(similarity.anti) | similarity > similarity.anti, !word %in% antonyms) |>
      dplyr::select(-similarity.anti)
  }
  similarities
}

#' Expand words using wildcard expansion
#' Default uses 'glob' style wildcards ("finan*"), set type='regex' to use
#' regular expressions
#' @param dictionary a character vector of words containing wildcards
#' @param vectors A vectors object, e.g. as returned by load_fasttext
#' @return a character vector containing the expanded terms
#' @export
expand_wildcards = function(dictionary, vectors, type=c("glob","regex")) {
  type = match.arg(type, c("glob","regex"))
  if (type == "glob") dictionary = glob2rx(dictionary)
  pattern = paste0("(?:", dictionary, ")", collapse="|")
  stringr::str_subset(vectors$vocabulary$word, pattern)
}



#' Use word embedding to automatically expand seed set and compute metrics
#'
#' This will split the terms in train and test sets, use `nearest_neighbours`
#' to generate similar words, and test whether the generated words
#' occur in the test set, giving a pseudo-precision and pseudo-recall.
#' Note that both will be lower than actualy precision and recall as we assume
#' the seed words are incomplete (why else expand it), but it can serve as an
#' indication of how many terms to consider.
#'
#' @param dictionary a character vector of words containing wildcards
#' @param vectors A vectors object, e.g. as returned by load_fasttext

#' @param ft_model a FastTextR model
#' @param words a character vector of words
#' @param split Faction to use as training data
#' @param vocabulary If given, limit results to words from this vocabulary
#'                   (e.g. only words occurring in the target corpus)
#' @param k Number of candidates to investigate
#' @return A tibble containing terms and (pseudo-)metrics
#' @export
find_expansion_threshold = function(dictionary, vectors, split=.5, k=1000)  {
  train = sample(terms, split*length(terms))
  test = setdiff(terms, train)
  candidates = nearest_neighbors(ft_model, train, k)
  candidates = names(candidates)
  if (!is.null(vocabulary)) candidates = intersect(candidates, vocabulary)
  evaluate_candidates(candidates, test)
}

