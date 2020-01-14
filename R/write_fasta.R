
#' Write a fasta file compatible with DADA2
#'
#' This function takes a database (a tibble returned by \code{get_diatbarcode})
#' and returns (write) a fasta file compatible with DADA2
#'
#' @param x a tibble (the reference database)
#' @param col_tax a character vector giving the column names of the database to use to build the reference taxonomy
#' @param col_seq the column of the database corresponding to the DNA sequences
#'
#' @return Silently returns the file content.
#' @export
#'
write_fasta_dada2 <- function(x, file = NULL,
                              col_tax = c("empire", "kingdom", "subkingdom", "phylum",
                                          "class", "order", "family", "genus", "species"),
                              col_seq = "sequence") {
  tax <- apply(x[, col_tax],
               1, paste0, collapse = ";")
  tax <- paste0(">", tax, ";")

  seq <- toupper(x[, col_seq, drop = TRUE])

  res <- paste(tax, seq, sep = "\n", collapse = "\n")

  if(!is.null(file)){
    writeLines(res, file.path(file))
  }

  invisible(res)
}

