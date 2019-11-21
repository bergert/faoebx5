#' @title Update rows in EBX5 Group
#'
#' @description This function aimed to update
#' data rows into a group stored in EBX5 through R.
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
UpdateEBXGroup <- function(data, sdmx_name) {

  #-- update to EBX5
  return ( update(data, sdmx_name, verb='update', isCodeList = FALSE))


}
