
dic_version <- function() {
  dic <- read.csv("http://www.francoiskeck.fr/work/dic_version.csv", header = TRUE, stringsAsFactors = FALSE)
  return(dic)
}


#' Get the \code{diat.barcode} database
#'
#' This function downloads and returns the \code{diat.barcode} database from the official server.
#'
#' @param version a character string giving the version. Use \code{"last"} (default) to get the last version available.
#' @param verbose a logical. Set to \code{FALSE} hide the automatic message. Default is \code{TRUE}.
#'
#' @return A \code{tibble} object.
#'
#' @details
#' The diat.barcode database is a library of barcodes which is curated and
#' which guarantees good taxonomical homogeneity and good quality of sequences.
#' Data have different sources:
#' \itemize{
#'   \item The culture collection of Thonon, INRA, TCC.
#'   \item The barcoding project of diatoms in UK.
#'   \item Public data (NCBI).
#' }
#' Several European labs participate in this curation:
#' \itemize{
#'   \item INRA – UMR Carrtel, FR–74200 Thonon–les–Bains, France.
#'   \item Botanischer Garten und Botanisches Museum Berlin–Dahlem, Freie Universität Berlin, Germany.
#'   \item Swedish University of Agricultural Sciences, Department of Aquatic Sciences and Assessment, Uppsala, Sweden.
#'   \item Bowburn Consultancy, Durham, UK.
#'   \item Royal Botanic Garden Edinburgh, Edinburgh, Scotland, UK.
#'   \item Marine and Continental Waters, Institute for Food and Agricultural Research and Technology (IRTA), Catalonia, Spain.
#'   \item Laboratory for Evolutionary Ecology, center for Marine Research, Ruder Boskovic Institute, Rovinj, Croatia.
#' }
#' @export
#'
get_diatbarcode <- function(version = "last", verbose = TRUE){
  dic <- dic_version()
  version <- as.character(version)
  if(version == "last") {
    version <- dic$Version[which.max(as.numeric(as.POSIXlt(dic$Date, format = "%d-%m-%Y")))]
  }
  dv <- dic[dic$Version == version, , drop = TRUE]

  dv$db_name <- ifelse(dv$Version %in% as.character(1:6), "R-syst", "Diat.barcode")

  httr::GET(dv$URL,
            httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  dat <- readxl::read_xlsx(tf, sheet = 1, guess_max = 10^7)

  if(verbose){
    cat("Hey! This is ", dv$db_name," v.", dv$Version, " published on ", dv$Date, ".\n", sep = "")
    cat("This database is distributed under the terms of the Open Licence 2.0.\n")
    cat("Consult: https://www.etalab.gouv.fr/wp-content/uploads/2018/11/open-licence.pdf\n")
  }

  return(dat)
}

