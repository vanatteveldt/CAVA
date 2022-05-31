#' Compute most similar words to a dictionary of seed words
#'
#' @param dictionary a character vector of words containing wildcards
#' @param vectors A vectors object, e.g. as returned by load_fasttext
#' @param k the (maximum) number of neighbours to return
#' @return along-format tibble containing columns word and dist
#' @export
similar_words = function(dictionary, vectors, antonyms=NULL) {
  centroid <- if (length(vectors)>1) get_centroid(dictionary, vectors) else vectors
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
  dplyr::left_join(similarities, vectors$vocabulary, by="word")
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
  if (type == "glob") dictionary = utils::glob2rx(dictionary)
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
#' Note that the seed set will be passed through `expand_wildcards` after
#' the train/test split.
#' If using wildcards, it might be better to provide the seed set
#' before expansion as otherwise very similar terms 
#' might be in the train and seed set
#'
#' @param dictionary a character vector of words containing wildcards
#' @param vectors A vectors object, e.g. as returned by load_fasttext

#' @param ft_model a FastTextR model
#' @param words a character vector of words
#' @param split Faction to use as training data
#' @param vocabulary If given, limit results to words from this vocabulary
#'                   (e.g. only words occurring in the target corpus)
#' @param k Number of candidates to investigate
#' @param n Number of times to resample/repeat the evaluation  
#' @return A tibble containing terms and (pseudo-)metrics
#' @export
evaluate_expansion = function(seed, vectors, split=.5, n=10)  {
  f = function(prec, rec, beta=1) {
    ifelse(prec+rec==0, 0, (1+beta^2)*prec*rec / (beta^2*prec + rec))
  }
  n_sample = if (split >= 1) split else split*length(seed)
  do_evaluate = function(seed, vectors, n_sample, k) {
    train = sample(seed, n_sample) 
    test = setdiff(seed, train) |> expand_wildcards(vectors)
    train = expand_wildcards(train, vectors)
    message(glue::glue("|train|={length(train)}, |test|={length(test)}"))
    similar_words(train, vectors) |>
      arrange(desc(similarity)) |>
      mutate(rank=row_number(),
             .in_test = as.numeric(word %in% test),
             .c=cumsum(.in_test),
             precision=.c/rank,
             recall=.c/length(test),
             f4=f(precision, recall, beta=4),
             .c=NULL,
             .in_test=NULL,
             frequency=NULL
             )
  }
 purrr::map(1:n, function(x) do_evaluate(seed, vectors, n_sample, k)) |>
   bind_rows()  |>
   group_by(word) |> 
   summarize(across(similarity:f4, list(mean=mean, sd=sd)))
}


