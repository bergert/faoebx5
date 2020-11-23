#' @title Read EBX5 Code List
#'
#' @description This function reads a table from EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param branch dataset branch
#' @param instance dataset instance
#' @param folder folder where the table is
#' @param folder2 optional second level folder
#' @param table ebx5 table name
#' @param connection the result of \code{\link{GetEBXConnection}}; optional
#'
#' @seealso \code{\link{GetEBXConnection}}.
#'
#' @return Return an object of the class \code{\link[data.table]{data.table}}.
#'
#' @details
#' Using the SOAP-API the tabe table name is not the label visible in EBX, but the name.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#'
#' @importFrom RCurl basicTextGatherer parseHTTPHeader curlPerform
#' @importFrom XML getNodeSet xmlToDataFrame xmlParse
#' @import data.table
#'
#' @examples
#' \dontrun{
#' EBXRead(branch='Fishery',
#'           instance='Fishery',
#'           folder='Metadata',
#'           table='Test_table')
#' }
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
#'
EBXRead <- function(branch, instance, folder, folder2='', table, connection = NA) {

  #-- connection details ----
  if (missing(connection) || is.na(connection)) {
    connection <- GetEBXConnection()
  }
  if(missing(branch) || missing(instance)) {
    stop('Please, specify branch and instance for ', table)
  }
  if(missing(folder) || missing(table)) {
    stop('Please, specify folder and table for ', table)
  }

  ##-- SOAP: Header ----
  headerFields <- header_fields('select')

  ##-- Body: request ----
  body <- soap_request_read(
    .user     = connection$username,
    .secret   = connection$password,
    .verb     = 'select',
    .table    = table,
    .branch   = branch,
    .instance = instance)

  ##-- API request ----
  reader <- basicTextGatherer()
  header <- basicTextGatherer()

  curlPerform(url = connection$ebx_soap_url,
              httpheader = headerFields,
              postfields = body,
              writefunction = reader$update,
              headerfunction = header$update)

  ##-- Status ----
  h <- parseHTTPHeader(header$value())
  if(!(h['status'] >= 200 & h['status'] <= 300)) {

    doc <- xmlParse(reader$value())
    df  <- xmlToDataFrame(getNodeSet(doc, "//SOAP-ENV:Fault"), stringsAsFactors = F)
    msg <- paste(names(df), ": ", df[1,], collapse = "\n", sep = '')

    stop('Please, check if you have permission to access this data.\n\n',
         'Details:\n', msg)
  }

  ##--- Converting XML object to dataframe ----
  doc <- xmlParse(reader$value())
  df  <- xmlToDataFrame(nodes = getNodeSet(doc, paste0("//", table)), stringsAsFactors = F)
  attr(df, ".internal.selfref") <- NULL

  return(df)
}
