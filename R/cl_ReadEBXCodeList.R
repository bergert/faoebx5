#' @title Read EBX5 Code List
#'
#' @description This function reads a code list from EBX5 to R.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param sdmx_code_list_name code list name, in SDMX style. Please, see
#' the code lists available by running the function \code{\link{GetEBXCodeLists}} in the field "Acronym".
#' The actual codelist location in EBX5 (branch, instance, code-list-name) are resolved
#' using the metadata structure.
#'
#' @seealso \code{\link{GetEBXCodeLists}}.
#'
#' @return Return an object of the class \code{\link[data.table]{data.table}}.
#'
#' @importFrom RCurl basicTextGatherer parseHTTPHeader curlPerform
#' @importFrom keyring key_get
#' @importFrom XML getNodeSet xmlToDataFrame xmlParse
#' @import data.table
#'
#' @examples
#' \dontrun{
#' ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_ISSCFC")
#' }
#' @export
#'
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
ReadEBXCodeList <- function(sdmx_code_list_name) {

  if(missing(sdmx_code_list_name)) {
    stop('Please, provide the code list name.')
  }

  #-- connection details ----
  connection <- GetEBXConnection()

  #-- read metadata; if not already loaded
  if(!exists("ebx5.cl_data")) {
    ebx5.cl_data <<- GetEBXCodeLists(connection)
  }

  if (length(ebx5.cl_data$Name[ebx5.cl_data$Acronym == sdmx_code_list_name]) == 0) {
    stop('Cannot find a codelist with acronym=<', sdmx_code_list_name, '> defined in EBX metadata')
  }

  #-- resolve the acutal location using metadata ----
  branch <- as.character(ebx5.cl_data$Branch[ebx5.cl_data$Acronym == sdmx_code_list_name])
  instance <- as.character(ebx5.cl_data$Instance[ebx5.cl_data$Acronym == sdmx_code_list_name])
  cl_name <- as.character(ebx5.cl_data$Name[ebx5.cl_data$Acronym == sdmx_code_list_name])

  if (is.na(branch) | is.na(instance) | is.na(cl_name)) {
    stop('Cannot find branch,instance for ', sdmx_code_list_name)
  }

  #-- read from EBX5
  return (getCodeList(connection, branch, instance, cl_name))
}
