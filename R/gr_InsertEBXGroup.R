#' @title Insert rows in EBX5 Group
#'
#' @description This function aimed to insert
#' data rows into a group stored in EBX5 through R.
#'
#' @param data a \code{\link[base]{data.frame}} that will be appended.
#' @param folder folder name in EBX that the code list is stored.  Please, see
#' the code list options by running the function \code{\link{GetEBXGroups}}
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
InsertEBXGroup <- function(data, sdmx_name) {

  #-- insert to EBX5
  return ( update(data, sdmx_name, verb='insert', isCodeList = FALSE))

}
