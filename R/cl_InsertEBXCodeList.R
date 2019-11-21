#' @title Insert rows in EBX5 Code List
#'
#' @description This function aimed to insert
#' data rows into a code list stored in EBX5 through R.
#'
#' @param data a \code{\link[base]{data.frame}} that will be appended.
#' @param folder folder name in EBX that the code list is stored.  Please, see
#' the code list options by running the function \code{\link{GetEBXCodeLists}} (\code{\link[base]{character}}).
#'
#' @inheritParams ReadEBXCodeList
#'
#' @return boolean
#'
#' @details Note that the new rows must have the same columns name os the table that will be appended.
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
#' InsertEBXCodeList(data     = cl_new,
#'                   sdmx_name  = 'CL_FI_COUNTRY_ITEM')
#' }
#'
#' @export
#'
#' @importFrom XML addChildren
#' @importFrom methods as
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
InsertEBXCodeList <- function(data, sdmx_name) {

  #-- update to EBX5
  return ( update(data, sdmx_name, verb='insert', isCodeList = TRUE))

}
