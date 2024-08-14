#' Construct a network among cue words from word association data
#'
#' @param x A cue by response matrix, or a data frame with cues and responses as
#'   the first and second columns, respectively.
#' @param value_var,cue_var,resp_var If \code{x} is a data.frame, the names
#'   of these three critical columns need to be provided.
#' @returns An igraph object encoded a cue->cue directed network with no loops.
#'   Nodes correspond to cues that also appear as responses; only responses that are
#'   also used as cues factor in to the network structure.
#'
#' @details
#'   The following is a block quote from De Deyne (2018) Behav. Res. Methods.
#'
#'   "As usually formulated, an association network is a graph in which each
#'   node corresponds to a word, and an edge connects nodes i and j if any
#'   person produces word j as a response when presented with word i as a cue
#'   (De Deyne & Storms, 2008a; De Deyne et al., 2016; Dubossarsky et al., 2017;
#'   Steyvers & Tenenbaum, 2005, see for instance). There is some variation in
#'   how these networks are constructed. Sometimes the edges are directed,
#'   reflecting the fact that word associations are often asymmetric, while
#'   other studies do not use this information. Similarly, edges are sometimes,
#'   but not always, weighted, in order to reflect the frequency with which word
#'   j appeared as a response to word i. It is also commonplace to include only
#'   those words that appeared as cues within the network, which produces loss
#'   of data which might bias other quantities derived from this network (for
#'   instance, the number of incoming links; see De Deyne et al., (2014)).
#'   Finally, it is typical to retain only the largest strongly connected
#'   component. This ensures that only those words that have both ingoing and
#'   outgoing edges are retained and that there is a path connecting all
#'   possible pairs of words in the graph."
#'
#'   The \code{network_of_cues()} and \code{strong_component()} functions
#'   produce directed, weighted, strongly-connected networks among cue words
#'   based on association data.
#'
#' @export
network_of_cues <- function(x, ...) {
  UseMethod("network_of_cues", x)
}

#' @export
network_of_cues.data.frame <- function(x, value_var, cue_var = "cue", resp_var = "response") {
  df <- x[c(cue_var, resp_var, value_var)]
  names(df)[3] <- "weight"
  cues <- unique(df[[cue_var]])
  responses <- unique(df[[resp_var]])
  w <- intersect(cues, responses)
  df <- df[(df$cue %in% w) & (df$response %in% w), ]
  df <- df[order(df$response, df$cue), ]
  g_digraph <- igraph::graph_from_data_frame(df, directed = TRUE)
  g <- g_digraph |>
    igraph::induced_subgraph(
      degree(g_digraph, mode = "out") > 0
    ) |>
    igraph::simplify(
      remove.multiple = FALSE,
      remove.loops = TRUE
    )
  return(g)
}


#' @export
network_of_cues.matrix <- network_of_cues.dgCMatrix <- function(x) {
  w <- sort(intersect(rownames(x), colnames(x)))
  x <- x[w, w]
  G <- igraph::graph_from_adjacency_matrix(x, mode = "directed", diag = FALSE, weighted = TRUE)
  return(G)
}


#' Identify and extract the largest strong component from a network
#'
#' @param g An igraph representation of a simple network of cues
#' @returns The largest set of nodes with in- and out-degree greater than 0.
#' @export
strong_component <- function(g) {
  stopifnot(igraph::is.igraph(g))
  comp <- igraph::components(g, mode = "strong")
  maxComp <- which.max(comp$csize)
  maxSize <- max(comp$csize)
  componentSizes <- comp$csize
  removedVertices <- names(comp$membership[comp$membership != maxComp])
  return(igraph::induced_subgraph(g, comp$membership == maxComp))
}

