#' @title Remove rows in EBX5 Code List
#'
#' @description This function deletes data rows from a code list stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param data a \code{\link[base]{data.frame}} that will be inserted.
#' @param sdmx_codelist_name codelist name see Acronym \code{\link{GetEBXCodeLists}} (\code{\link[base]{character}}).
#'
#' @inheritParams InsertEBXCodeList
#'
#' @return boolean
#'
#' @details For a delete operation, data must specify all keys of the code-list to uniqely idetify the rows.
#' Using the SOAP-API the column name is not the label visible in EBX, but the field name.
#'
#' @importFrom XML addChildren
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
RemoveEBXCodeList <- function(data, sdmx_codelist_name) {

  #-- update to EBX5
  return ( update(data, sdmx_codelist_name, verb='delete', isCodeList = TRUE))
}
