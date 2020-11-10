#' @title Get EBX5 Code Lists
#'
#' @description This function returns the list of code lists
#' defined in the 'Metatadata' structure in EBX5. Metadata contains
#' the instance, branch and the code-list name used in EBX5.
#' This way, the software uses the SDMX-style codelist name, and the actual
#' location inside EBX5 can be changed without breaking the software.
#' \code{\link{ReadEBXCodeList}}.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param connection is optional
#'
#' @seealso \code{\link{ReadEBXCodeList}} \code{\link{GetEBXGroups}}
#'
#' @return Returns an object of the class \code{\link[data.table]{data.table}}
#'
#' @importFrom RCurl basicTextGatherer parseHTTPHeader curlPerform
#' @importFrom keyring key_get
#' @importFrom XML getNodeSet xmlToDataFrame xmlParse
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' GetEBXCodeLists()
#' }
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
GetEBXCodeLists <- function(ebx5_connection) {

  #-- EBX5: connection ----

  if(missing(ebx5_connection)) {
    ebx5_connection <- GetEBXConnection()
  }

  #-- read metadata::EBXCodelist ----
  return (getCodeList(ebx5_connection, ebx5_connection$meta_branch, ebx5_connection$meta_instance, 'EBXCodelist'))
}

