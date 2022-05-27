#' Compute the pairwise similarities between all words in a dictionary
#'
#' @param dictionary A character vector of words
#' @param vectors A vectors object, e.g. as returned by load_fasttext
#' @returns a (tibble) data frame with columns word1, word2, sim, frequency1 and frequency2
#' @export
pairwise_similarities <- function(dictionary, vectors) {
  v <- vectors$vectors[rownames(vectors$vectors) %in% dictionary,]
  v %*% t(v) |>
    tibble::as_tibble(rownames="word1") |>
    tidyr::pivot_longer(-word1, names_to="word2", values_to="similarity") |>
    dplyr::filter(word2 > word1) |>
    dplyr::arrange(-similarity) |>
    dplyr::left_join(vectors$vocabulary, by=c("word1"="word")) |>
    dplyr::left_join(vectors$vocabulary, by=c("word2"="word"), suffix=c("1", "2"))
}

#' Create a similarity graph
#' @param similarities a data frame of word-word similarities (e.g. output from pairwise_similarity)
#' @param threshold the threshold for including a relation in the graph
#' @return an igraph graph object
#' @export
similarity_graph <- function(similarities, threshold=NULL, max_edges=NULL) {
  if (!is.null(threshold))
    similarities <- dplyr::filter(similarities, similarity >= threshold)
  if (!is.null(max_edges))
    similarities <- utils::head(similarities, n=max_edges)

  v = dplyr::bind_rows(similarities |> dplyr::select(word=word1, n=frequency1),
                      similarities |> dplyr::select(word=word2, n=frequency2)) |>
    unique()
  similarities <- dplyr::select(similarities, word1, word2, similarity)
  g <- igraph::graph_from_data_frame(similarities, directed=F, vertices=v)
  g <- igraph::delete_edges(g, igraph::E(g)[igraph::E(g)$similarity<threshold])
  igraph::E(g)$width <- 1 + scales::rescale(igraph::E(g)$similarity)^3*10
  igraph::V(g)$shape <- "none"
  igraph::V(g)$label.cex <- .5 + scales::rescale(log(igraph::V(g)$n))
  igraph::V(g)$cluster <- igraph::membership(igraph::cluster_walktrap(g, steps = 6))
  igraph::V(g)$label.color <- igraph::V(g)$cluster
  return(g)
}

#' Compute the normalized centroid vector for the given dictionary
get_centroid <- function(dictionary, vectors) {
  v <- vectors$vectors[rownames(vectors$vectors) %in% dictionary, ,drop=F]
  centroid <- colMeans(v)
  centroid / sqrt(sum(centroid^2))
}

#' Compute the similarity of each word in the dictionary to the centroid
#'
#' Can be useful to find words that don't seem to belong in the dictionary
#'
#' @param vectors A vectors object, e.g. as returned by load_fasttext
#' @param dictionary a character vector of words
#' @return a long-format tibble containing columns word and dist
#' @export
similarity_to_centroid <- function(dictionary, vectors) {
  centroid <- get_centroid(dictionary, vectors)
  v <- vectors$vectors[rownames(vectors$vectors) %in% dictionary,]
  similarities <- (v %*% centroid)[,1]
  tibble::tibble(word=names(similarities), similarity=similarities) |>
    dplyr::arrange(similarity) |>
    dplyr::left_join(vectors$vocabulary)
}
