#' @title Update rows in EBX5 Group
#'
#' @description This function aimed to update
#' data rows into a group stored in EBX5 through R.
#'
#' @param data a \code{\link[base]{data.frame}} containing all columns to be be updated
#' @param sdmx_group_name group name, in SDMX style.
#' Available code lists are shown by function \code{\link{GetEBXGroups}} in the field "Acronym".
#' The actual group location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#'
#' @inheritParams InsertEBXGroup
#'
#' @return boolean
#'
#' @details Note that the udpated rows must have the same columns name os the table that will be updated.
#'
#' @examples
#'
#' \dontrun{
#' UpdateEBXGroup(data = gr_update,
#' gr_name = 'EBXGroup',
#' folder = 'Metadata')
#' }
#'
#' @export
#'
#' @importFrom XML addChildren
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
UpdateEBXGroup <- function(data, sdmx_group_name) {

  #-- update to EBX5
  return ( update(data, sdmx_group_name, verb='update', isCodeList = FALSE))


}
