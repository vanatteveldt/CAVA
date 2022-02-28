#' Load similarity information from a fasttext model
#'
#' @param filename a filename pointing to a fasttext .bin model
#' @param corpus an optional corpus to use for word frequency information.
#'               if given, should be a quanteda corpus, dfm or data frame with word and frequency as first two columns
#'
#' @returns a list with a nxn similarity matrix and a frequency data frame
#' @export
load_fasttext <- function(filename, corpus=null, n=10000) {
  pb <- progress::progress_bar$new(total = 5, format=":what [:bar] :eta")
  pb$tick(tokens=list(what="Loading fasttext model"))
  if (!requireNamespace("fastTextR", quietly = TRUE)) stop("load_fasttext requires the fastTextR module")
  filename <- path.expand(filename)
  if (!file.exists(filename)) stop(paste0("File ", filename, " does not exist"))

  ft_model = fastTextR::ft_load(filename)
  pb$tick(tokens=list(what="Extracting vocabulary"))
  words <- ft_model |>
    fastTextR::ft_words() |>
    stringr::str_subset("^[a-z]")

  if (!is.null(corpus)) {
    pb$tick(tokens=list(what="Converting corpus"))
    vocabulary <- load_corpus(corpus) |>
      dplyr::filter(word %in% words) |>
      utils::head(n=n)
  } else {
    pb$tick(tokens=list(what="Estimating ranks"))
    vocabulary <- data.frame(words=words[1:n], frequency=.5^(0:(n-1)))
  }

  pb$tick(tokens=list(what="Extracting vectors"))
  vectors <- fastTextR::ft_word_vectors(ft_model, vocabulary$word)
  pb$tick(tokens=list(what="Normalizing vectors"))
  vectors <- vectors / sqrt(rowSums(vectors^2))
  pb$terminate()
  list(vectors=vectors, vocabulary=vocabulary)
}


#' Get frequency information from a corpus
load_corpus <- function(corpus) {
  if (methods::is(corpus, "corpus")) corpus = corpus |>
      quanteda::tokens(remove_punct=T, remove_numbers=T, remove_symbols=T) |>
      quanteda::dfm()
  if (methods::is(corpus, "dfm")) corpus = corpus |>
      quanteda.textstats::textstat_frequency() |>
      tibble::as_tibble()
  if (!methods::is(corpus, "data.frame")) stop("corpus should be a corpus, dfm, or data.frame")
  if (!colnames(corpus)[1] %in% c("word", "feature")) warning("Using first column of corpus ('", colnames(corpus)[1], "') as words")
  if (!colnames(corpus)[2] %in% c("n", "frequency")) warning("Using second column of corpus ('", colnames(corpus)[2], "') as frequency")
  corpus |>
    dplyr::select(word=1, frequency=2) |>
    dplyr::arrange(-frequency)
}

