#' @title Remove rows in EBX5 Group
#'
#' @description This function aimed to remove
#' data rows into a group stored in EBX5 through R.
#'
#' @inheritParams InsertEBXGroup
#'
#' @return boolean
#'
#' @importFrom XML addChildren
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
RemoveEBXGroup <- function(data, sdmx_name) {

  #-- delete from EBX5
  return ( update(data, sdmx_name, verb='delete', isCodeList = FALSE))

}
