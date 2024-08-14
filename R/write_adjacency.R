#' Write adjacency matrix
#' @export
write_adjacency <- function(g, dataset) {
  labs <- igraph::V(g)$name
  rc <- igraph::as_edgelist(g, names = FALSE)
  df <- cbind(rc, igraph::E(g)$weight)
  write.table(
    df,
    file = paste0(dataset,"_adj.tsv"),
    sep = "\t",
    row.names = F,
    col.names = F
  )
  write.table(
    labs,
    file = paste0(dataset, "_labels.txt"),
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
}

#f <- file("~/../Documents/SWOWEN-R123-S_rw-double-12216x12216.bin", open = "rb")
#S_rw <- matrix(readBin(f, what = "double", n = 12216^2), nrow = 12216, ncol = 12216)
#close(f)
#
#f <- file("~/../Documents/SWOWEN-R123-S_rw-single-12216x12216.bin", open = "rb")
#S_rw_single <- matrix(readBin(f, what = "numeric", n = 12216^2, size = 4), nrow = 12216, ncol = 12216)
#close(f)
#
#f <- file("~/../Documents/SWOWEN-R123-G_rw-double-12216x12216.bin", open = "rb")
#G_rw <- matrix(readBin(f, what = "double", n = 12216^2), nrow = 12216, ncol = 12216)
#close(f)
#
#f <- file("~/../Documents/SWOWEN-R123-G_rw-single-12216x12216.bin", open = "rb")
#G_rw_single <- matrix(readBin(f, what = "numeric", n = 12216^2, size = 4), nrow = 12216, ncol = 12216)
#close(f)
#
