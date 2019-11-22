#' @title Get EBX Groups
#'
#' @description This function returns the list of groups
#' defined in the 'Metatadata' structure in EBX5. Metadata contains
#' the instance, branch and the code-list name used in EBX5.
#' This way, the software uses the SDMX-style group name, and the actual
#' location inside EBX5 can be changed without breaking the software.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param connection is optional
#'
#' @seealso \code{\link{ReadEBXGroup}} \code{\link{GetEBXCodeLists}}
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
#' GetEBXGroups()
#' }
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
GetEBXGroups <- function(connection) {

  #-- EBX5: connection ----

  if(missing(connection)) {
    connection <- GetConnection()
  }

  #-- read metadata::EBXCodelist ----
  return (getCodeList(connection, connection$meta_branch, connection$meta_instance, 'EBXGroup'))

  return(df)
}
