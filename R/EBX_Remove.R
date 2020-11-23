#' @title Remove rows in EBX5 Code List
#'
#' @description This function deletes a single data row from a code list stored in EBX5.
#'
#' @param branch dataset branch
#' @param instance dataset instance
#' @param table ebx5 table name
#' @param data a \code{\link[base]{data.frame}} that will be removed.
#' data must specify the unique primary key(s); which identify the row to be removed.
#' @param connection the result of \code{\link{GetEBXConnection}}; optional
#'
#' @return boolean
#'
#' @details All fields of a primary key (usually the Identifier) must be present.
#' Using the SOAP-API the column names and tabe table name are not the label visible in EBX, but the name.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @examples
#' \dontrun{
#' cl_remove <- data.frame(
#' Identifier = c(999))
#'
#'EBXRemove( branch   = 'Fishery',
#'           instance = 'Fishery',
#'           table    = 'Test_table',
#'           data     = cl_remove)
#' }
#'
#' @importFrom XML addChildren
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
EBXRemove <- function(branch, instance, table, data, connection = NA) {

  #-- connection details ----
  if (missing(connection) || is.na(connection)) {
    connection <- GetEBXConnection()
  }
  if(missing(branch) || missing(instance)) {
    stop('Please, specify branch and instance for ', table)
  }
  if (missing(table)) {
    stop('Please, specify folder and table for ', table)
  }

  if (ncol(data) != 1) {
    stop('sorry, only one key column is supported right now', ncol(data))
  }

  ## -- create the Xpath statement --
  v_predicate <- lapply(data, create_predicate, names(data[1]))
  predicate <- lapply(v_predicate, function (i) paste(i, collapse = ' or '))

  ##-- SOAP: Header ----
  headerFields <- header_fields('delete')

  ##-- Body: request ----
  predicate <-
  body <- soap_request_delete(
    .user      = connection$username,
    .secret    = connection$password,
    .branch    = branch,
    .instance  = instance,
    .table     = table,
    .predicate = predicate[[1]])

  ##-- API request ----
  reader <- basicTextGatherer()
  header <- basicTextGatherer()

  curlPerform(url = connection$ebx_soap_url,
              httpheader = headerFields,
              postfields = body,
              writefunction = reader$update,
              headerfunction = header$update,
              .encoding = "UTF-8")

  ##-- Status ----
  h <- parseHTTPHeader(header$value())
  if(!(h['status'] >= 200 & h['status'] <= 300)) {

    doc <- xmlParse(reader$value())
    df  <- xmlToDataFrame(getNodeSet(doc, "//SOAP-ENV:Fault"), stringsAsFactors = F)
    msg <- paste(names(df), ": ", df[1,], collapse = "\n", sep = '')

    stop('Please, check if you have permission to access this data.\n\n',
         'Details:\n', msg)
  } else{

    return(TRUE)

  }
}


create_predicate <- function (keyvalue, xpath) {
  sprintf('./%s = %s', xpath , keyvalue)
}
