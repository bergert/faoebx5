#' @title Read EBX5 Group
#'
#' @description This function reads group data from EBX5 to R.
#'
#' @param sdmx_group_name group name, in SDMX style. Please, see
#' the groups available by running the function \code{\link{GetEBXGroups}} in the field "Acronym".
#' The actual group location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#'
#' @seealso \code{\link{GetEBXGroups}}.
#'
#' @return Return an object of the class \code{\link[data.table]{data.table}}.
#'
#' @importFrom RCurl basicTextGatherer parseHTTPHeader curlPerform
#' @importFrom keyring key_get
#' @importFrom XML getNodeSet xmlToDataFrame xmlParse
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' gr <- ReadEBXGroup(sdmx_group_name = 'HCL_FI_COMMODITY_FAOL1_FAOL2')
#' }
#'
#' @export
#'
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
ReadEBXGroup <- function(sdmx_group_name) {

  if(missing(sdmx_group_name)) {
    stop('Please, provide the group name.')
  }

  #-- connection details ----
  connection <- GetConnection()

  #-- read metadata; if not already loaded
  if(!exists("ebx5.gr_data")) {
    ebx5.gr_data <<- GetEBXGroups()
  }

  if (length(ebx5.gr_data$Name[ebx5.gr_data$Acronym == sdmx_group_name]) == 0) {
    stop('Cannot find a group with acronym=<', sdmx_group_name, '> defined in EBX metadata')
  }

  branch   <- as.character(ebx5.gr_data$Branch[ebx5.gr_data$Acronym == sdmx_group_name])
  instance <- as.character(ebx5.gr_data$Instance[ebx5.gr_data$Acronym == sdmx_group_name])
  gr_name  <- as.character(ebx5.gr_data$Name[ebx5.gr_data$Acronym == sdmx_group_name])

  if (is.na(branch) | is.na(instance) | is.na(gr_name)) {
    stop('Cannot find branch,instance for ', sdmx_group_name)
  }

  #-- read from EBX5--
  return (getCodeList(connection, branch, instance, gr_name))
}
