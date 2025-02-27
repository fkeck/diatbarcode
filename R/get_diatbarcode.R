
dic_version <- function() {
  dic <- read.csv("https://raw.githubusercontent.com/fkeck/diatbarcode/refs/heads/master/dic_version.csv", header = TRUE, stringsAsFactors = FALSE)
  return(dic)
}


#' Get the diat.barcode database
#'
#' This function downloads and returns the \code{diat.barcode} database from the official server.
#'
#' @param version a character string giving the version. Use \code{"last"} (default) to get the last version available.
#' @param clean_names a logical. If \code{TRUE} (default) the column names are cleaned and turned
#' to snake case by a call to janitor::clean_names.
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
#'
#' @references
#' Rimet, Frederic; Gusev, Evgenuy; Kahlert, Maria; Kelly, Martyn; Kulikovskiy, Maxim; Maltsev,
#' Yevhen; Mann, David; Pfannkuchen, Martin; Trobajo, Rosa; Vasselon, Valentin; Zimmermann, Jonas;
#' Bouchez, Agnès, 2018, "Diat.barcode, an open-access barcode library for diatoms",
#' \url{https://doi.org/10.15454/TOMBYZ}, Portail Data Inra, V1
#'
#' Rimet F., Chaumeil P., Keck F., Kermarrec L., Vasselon V., Kahlert M., et al. (2016).
#' R-Syst::diatom: an open-access and curated barcode database for diatoms and freshwater monitoring.
#' Database 2016, baw016. \url{https://doi.org/10.1093/database/baw016}
#'
#' @section Licence:
#' Diatbarcode and R-Syst are distributed under the terms of the Open Licence 2.0.
#' Consult: \url{https://www.etalab.gouv.fr/wp-content/uploads/2018/11/open-licence.pdf}
#'
#' @export
#'
get_diatbarcode <- function(version = "last", clean_names = TRUE, verbose = TRUE){

  dd <- download_diatbarcode(path = NULL, flavor = "original", version = version)

  dat <- readxl::read_xlsx(dd$path, sheet = 1, guess_max = 10^7)

  if(clean_names){
    dat <- janitor::clean_names(dat, case = "snake")
  }

  if(verbose){
    cat("Hey! This is ", dd$db_name," v.", dd$version, " published on ", dd$date, ".\n", sep = "")
    cat("This database is distributed under the terms of the Open Licence 2.0.\n")
    cat("Consult: https://www.etalab.gouv.fr/wp-content/uploads/2018/11/open-licence.pdf\n")
  }

  return(dat)
}


#' Download the diat.barcode database
#'
#' This function downloads the database in a given location on your computer.
#' The function allows to download different flavors of the original database.
#'
#' @param path the path where the downloaded file is to be saved. If \code{NULL}, the temporary directory.
#' @param flavor a flavor of the Diat.barcode database (see Details section).
#' @param version a character string giving the version. Use \code{"last"} (default) to get the last version available.
#'
#' @details
#'
#' The Diat.barcode database can be downloaded in different flavors. Currently available flavors are:
#' \itemize{
#'   \item \code{original} The original database.
#'   \item \code{rbcl312_dada2_tax} A pre-formated reference for taxonomic affiliation with DADA2.
#'   \item \code{rbcl312_dada2_spe} A pre-formated reference for exact species matching with DADA2.
#' }
#'
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
#'
#' @references
#' Rimet, Frederic; Gusev, Evgenuy; Kahlert, Maria; Kelly, Martyn; Kulikovskiy, Maxim; Maltsev,
#' Yevhen; Mann, David; Pfannkuchen, Martin; Trobajo, Rosa; Vasselon, Valentin; Zimmermann, Jonas;
#' Bouchez, Agnès, 2018, "Diat.barcode, an open-access barcode library for diatoms",
#' \url{https://doi.org/10.15454/TOMBYZ}, Portail Data Inra, V1
#'
#' Rimet F., Chaumeil P., Keck F., Kermarrec L., Vasselon V., Kahlert M., et al. (2016).
#' R-Syst::diatom: an open-access and curated barcode database for diatoms and freshwater monitoring.
#' Database 2016, baw016. \url{https://doi.org/10.1093/database/baw016}
#'
#' @section Licence:
#' Diatbarcode and R-Syst are distributed under the terms of the Open Licence 2.0.
#' Consult: \url{https://www.etalab.gouv.fr/wp-content/uploads/2018/11/open-licence.pdf}
#'
#'
#' @return Silently returns a list with informations about the downloaded file.
#' @export
#'
download_diatbarcode <- function(path = NULL, flavor = "original", version = "last"){

  dic <- dic_version()
  dic <- dic[dic$Flavor == flavor, ]

  if(nrow(dic) == 0) stop("Flavor ", flavor, " not found.")

  version <- as.character(version)
  if(version == "last") {
    version <- dic$Version[which.max(as.numeric(as.POSIXlt(dic$Date, format = "%d-%m-%Y")))]
  }

  dv <- dic[dic$Version == version, , drop = TRUE]

  if(is.data.frame(dv)) stop("Version ", version, " for ", flavor, " not found.")

  file_ext <- switch (flavor,
    original = ".xlsx",
    rbcl312_dada2_tax = ".fa.gz",
    rbcl312_dada2_spe = ".fa.gz"
  )

  if(is.null(path)){
    path <- tempfile(pattern = "diatbarcode_", fileext = file_ext)
  } else {
    path <- file.path(path, "diatbarcode_", flavor, version)
  }

  httr::GET(dv$URL, httr::write_disk(path))
  httr::GET(paste0("http://francoiskeck.fr/work/diatbarcode/dbc_counter_update.php?version=version_", dv$Version,
                   "&flavor=", dv$Flavor))

  res <- list()

  res$path <- path
  res$flavor <- dv$Flavor
  res$version <- dv$Version
  res$date <- dv$Date
  res$url <- dv$URL
  res$db_name <- ifelse(dv$Version %in% as.character(1:6), "R-syst", "Diat.barcode")

  invisible(res)
}
