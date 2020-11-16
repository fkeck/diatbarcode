[![Build Status](http://francoiskeck.fr/work/diatbarcode/button.png)](http://francoiskeck.fr/work/diatbarcode/dbc_counter.php)

# diatbarcode

The goal of the diatbarcode is to download the database Diat.barcode locally,
in order to use it directly within the R environment.

The diat.barcode database is a library of barcodes for diatoms and other algae which is curated and
which guarantees good taxonomical homogeneity and good quality of sequences.

More information on [the diatbarcode website](https://www6.inra.fr/carrtel-collection/Barcoding-database):

## Installation

You can install the package `diatbarcode` from GitHub with the `remotes` package (you need to install `remotes` first):

``` r
remotes::install_github("fkeck/diatbarcode")
```

## Example

This is an example which shows you how to download and load the database in R.

``` r
library(diatbarcode)
dbc <- get_diatbarcode(version = "last")
dbc
```

You can also use the function `download_diatbarcode` to download the database in a given location on your computer. The function allows to download different flavors of the original database. For example, `download_diatbarcode` is particularly useful to quickly download a pre-formated reference database for taxonomic affiliation with DADA2 (`flavor = "rbcl312_dada2_tax"`). See our [DADA2 pipeline](https://github.com/fkeck/DADA2_diatoms_pipeline) for an example.

``` r
library(diatbarcode)

# By default the file is saved in the temporary directory
dbc_dl <- download_diatbarcode(flavor = "rbcl312_dada2_spe")

# The function returns the path where the file was saved and other infos silently
dbc_dl
```
