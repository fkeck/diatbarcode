
dic_version <- function() {
  dic <- matrix(
    c("1",      "28-11-2012", "https://data.inra.fr/api/access/datafile/86731?gbrecs=true",
      "2",      "16-09-2014", "https://data.inra.fr/api/access/datafile/86732?gbrecs=true",
      "3",      "13-02-2015", "https://data.inra.fr/api/access/datafile/86733?gbrecs=true",
      "4",      "16-09-2015", "https://data.inra.fr/api/access/datafile/86734?gbrecs=true",
      "5",      "18-02-2016", "https://data.inra.fr/api/access/datafile/86735?gbrecs=true",
      "6",      "20-03-2017", "https://data.inra.fr/api/access/datafile/86736?gbrecs=true",
      "7",      "23-02-2018", "https://data.inra.fr/api/access/datafile/77781?gbrecs=true",
      "7.1",    "12-02-2019", "https://data.inra.fr/api/access/datafile/83042?gbrecs=true"
    ), ncol = 3, byrow = TRUE)
  colnames(dic) <- c("Version", "Date", "URL")
  dic <- as.data.frame(dic, stringsAsFactors = FALSE)
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

