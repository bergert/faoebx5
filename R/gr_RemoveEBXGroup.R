#' @title Remove rows in EBX5 Group
#'
#' @description This function removes data rows from a group stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param data a \code{\link[base]{data.frame}} that will be removed.
#' data must specify the unique primary key(s); which identify the rows to be removed.
#' @param sdmx_group_name group name, in SDMX style.
#' Available code lists are shown by function \code{\link{GetEBXGroups}} in the field "Acronym".
#' The actual group location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#'
#' @inheritParams InsertEBXGroup
#'
#' @return boolean
#'
#' @importFrom XML addChildren
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
RemoveEBXGroup <- function(data, sdmx_group_name) {

  #-- delete from EBX5
  return ( update(data, sdmx_group_name, verb='delete', isCodeList = FALSE))

}
