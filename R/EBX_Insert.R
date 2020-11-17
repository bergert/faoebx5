#' @title Insert rows in EBX5 Code List
#'
#' @description This function will insert data rows into a code list stored in EBX5.

#'
#' @inheritParams EBXRead
#' @param data a \code{\link[base]{data.frame}} containing all columns to be be inserted
#'
#' @return boolean
#'
#' @details The data columns provided must follow the code-list which is being inserted.
#' All fiels of a primary key (usually the Identifier), and all mandatory fields must be present.
#' Using the SOAP-API the column names and tabe table name are not the label visible in EBX, but the name.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @examples
#'
#' \dontrun{
#' cl_insert <- data.frame(Identifier = c(999, 888), Name_En=c('Test1','Test2'))
#' EBXInsert(branch='Fishery',
#'           instance='Fishery',
#'           folder='Metadata',
#'           table='Test_table',
#'           data=cl_insert)
#' }
#'
#' @export
#'
#' @importFrom XML addChildren xmlParse
#' @importFrom methods as
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
EBXInsert <- function(branch, instance, folder, folder2='', table, data, connection = NA) {

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
  headerFields <- header_fields()

  ##-- Body: request ----
  body <- body_request_data(.user     = connection$username,
                            .secret   = connection$password,
                            .branch   = branch,
                            .instance = instance,
                            .folder   = folder,
                            .folder2  = folder2,
                            .table    = table,
                            .verb     = 'insert')

  ##-- Building XML object----
  out_list <- data_convert_xml(.data = data, .table = table)
  body_xml <- xmlParse(body)
  if (is.na(folder2) || folder2=='') {
    metadata_xml <- getNodeSet(body_xml, paste0("//", folder))
  } else {
    metadata_xml <- getNodeSet(body_xml, paste0("//", folder2))
  }
  metadata_xml[[1]] <- addChildren(metadata_xml[[1]], kids = out_list)
  body_text <- as(body_xml, "character")

  ##-- API request ----
  reader <- basicTextGatherer()
  header <- basicTextGatherer()

  curlPerform(url = headerFields[['SOAPAction']],
              httpheader = headerFields,
              postfields = body_text,
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

  } else{
    return(TRUE)
  }
}
