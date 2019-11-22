#' @title Update rows in EBX5 Code List
#'
#' @description This function updates data rows of a code list stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @inheritParams InsertEBXCodeList
#'
#' @return boolean
#'
#' @details Note that the udpated rows must have the same columns name os the table that will be updated.
#'
#' @examples
#'
#' \dontrun{
#' cl_new <- data.frame(
#' Identifier = c(999, 888),
#' Acronym = 'TEST_ACRONYM',
#' Folder = 'TESTFOLDER',
#' Name = 'TEST_NAME',
#' Branch = 'Fishery',
#' Instance = 'Fishery')
#'
#' UpdateEBXCodeList(data     = cl_new,
#'                   cl_name  = 'EBXCodelist',
#'                   folder   = 'Metadata',
#'                   branch   = 'Fishery',
#'                   instance = 'Fishery')
#' }
#'
#' @export
#'
#' @importFrom XML addChildren
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
UpdateEBXCodeList <- function(data, sdmx_name) {

  #-- update to EBX5
  return ( update(data, sdmx_name, verb='update', isCodeList = TRUE))
}
