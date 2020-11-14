#' @title Insert rows in EBX5 Group
#'
#' @description This function inserts data rows into a group stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param data a \code{\link[base]{data.frame}} that will be appended.
#' @param sdmx_group_name group name, in SDMX style.
#' Available code lists are shown by function \code{\link{GetEBXGroups}} in the field "Acronym".
#' The actual group location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#'
#' @inheritParams ReadEBXGroup
#'
#' @seealso \code{\link{GetEBXGroups}}
#'
#' @details Note that the new rows must have the same columns name os the table that will be appended.
#'
#' @return boolean
#'
#' @examples
#'
#' \dontrun{
#' InsertEBXGroup(data = gr_new,
#' gr_name  = 'EBXGroup',
#' folder   = 'Metadata',
#' branch   = 'Fishery',
#' instance = 'Fishery')
#' }
#'
#' @export
#'
#' @importFrom XML addChildren
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
InsertEBXGroup <- function(data, sdmx_group_name) {

  #-- insert to EBX5
  return ( update(data, sdmx_group_name, verb='insert', isCodeList = FALSE))

}
