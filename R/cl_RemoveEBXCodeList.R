#' @title Remove rows in EBX5 Code List
#'
#' @description This function aimed to remove
#' data rows into a code list stored in EBX5 through R.
#'
#' @inheritParams InsertEBXCodeList
#'
#' @return boolean
#'
#' @importFrom XML addChildren
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
RemoveEBXCodeList <- function(data, sdmx_name) {

  #-- update to EBX5
  return ( update(data, sdmx_name, verb='delete', isCodeList = TRUE))
}
