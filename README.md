
# faoebx5 <img src="man/figures/logo.png" align="right" alt="" width="120" />

The `faoebx5` package provides low-level communication tools to read,
write, and update data stored in the \[EBX5 database\]. EBX5 is a
corporate repository for Master Data Management (MDM), it stores the
Single Soruce of Truth (SSOT) of all reference data. This library uses
the EBX5-SOAP API. For example, data from EBX5 is used to validate
input.

For an application-level interface with EBX5:
[fishstatR](https://bergert.github.io/fishstatr/):

## Installation

The latest development version can be installed from github:

``` r
install.packages("devtools")
devtools::install_github('bergert/faoebx5')
```

## Functions

`faoebx5` provides a set of functions to manage code lists and groups
data stored on EBX database. The collection of functions is split into
two groups, those to manage code list and those to control groups.
Please, see the
[Reference](https://bergert.github.io/faoebx5/reference/index.html) tab
for more details about the functions, or you can start with examples on
the page \[Get started
\](<https://bergert.github.io/faoebx5/articles/faoebx5.html>.
