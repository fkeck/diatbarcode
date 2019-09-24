# diatbarcode
[![Build Status](http://francoiskeck.fr/work/diatbarcode/button.png)](http://francoiskeck.fr/work/diatbarcode/dbc_counter.php)


The goal of the diatbarcode is to download the database Diat.barcode locally,
in order to use it directly within the R environment.

The diat.barcode database is a library of barcodes for diatoms and other algae which is curated and
which guarantees good taxonomical homogeneity and good quality of sequences.

More information on [the diatbarcode website](https://www6.inra.fr/carrtel-collection/Barcoding-database):

## Installation

You can install the package diatbarcode from GitHub with the devtools package (you need to install devtools first):

``` r
devtools::install_github("fkeck/diatbarcode")
```

## Example

This is an example which shows you how to load the database in R.

``` r
library(diatbarcode)
dbc <- get_diatbarcode(version = "last")
dbc
```

