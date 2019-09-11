
dic_version <- function() {
  dic <- matrix(
    c("1",      "28-11-2012", "https://data.inra.fr/api/access/datafile/86731?gbrecs=true",
      "2",      "16-09-2014", "https://data.inra.fr/api/access/datafile/86732?gbrecs=true",
      "3",      "13-02-2015", "https://data.inra.fr/api/access/datafile/86733?gbrecs=true",
      "4",      "16-09-2015", "https://data.inra.fr/api/access/datafile/86734?gbrecs=true",
      "5",      "18-02-2016", "https://data.inra.fr/api/access/datafile/86735?gbrecs=true",
      "6",      "20-03-2017", "https://data.inra.fr/api/access/datafile/86736?gbrecs=true",
      "7",      "23-02-2018", "https://data.inra.fr/api/access/datafile/77781?gbrecs=true",
      "7.1",    "2019-02-12", "https://data.inra.fr/api/access/datafile/83042?gbrecs=true"
    ), ncol = 3, byrow = TRUE)
  colnames(dic) <- c("Version", "Date", "URL")
  dic <- as.data.frame(dic, stringsAsFactors = FALSE)
  return(dic)
}

get_diatbarcode <- function(version){

  dic <- dic_version()
  dv <- dic[dic$Version == version, , drop = TRUE]

  httr::GET(dv$URL,
            httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  dat <- readxl::read_xlsx(tf, sheet = 1, )

}
