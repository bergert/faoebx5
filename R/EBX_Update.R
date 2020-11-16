#' @title Update rows in EBX5 Code List
#'
#' @description This function updates existing data rows of a code list stored in EBX5.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @param data a \code{\link[base]{data.frame}} containing the columns to be be updated
#'
#' @inheritParams EBXInsert
#'
#' @return boolean
#'
#' @details The data columns provided must follow the code-list which is being updated.
#' All fiels of a primary key (usually the Identifier) must be present.
#' Using the SOAP-API the column names and tabe table name are not the label visible in EBX, but the name.
#' Requires that the EBX5 connection was configured using \code{\link{SetupEBXConnection}}.
#'
#' @examples
#' \dontrun{
#' cl_update <- data.frame(
#'                Identifier = c(999, 888),
#'                Acronym = 'TEST_ACRONYM',
#'                Folder = 'TESTFOLDER',
#'                Name = 'TEST_NAME',
#'                Branch = 'Fishery',
#'                Instance = 'Fishery')
#' EBXUpdate(branch   = 'Fishery',
#'           instance = 'Fishery',
#'           folder   = 'Metadata',
#'           table    = 'Test_table',
#'           data     = cl_update)
#' }
#'
#' @export
#'
#' @importFrom XML addChildren
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
EBXUpdate <- function(branch, instance, folder, folder2='', table, data) {

  #-- connection details ----
  connection <- GetEBXConnection()
  if(missing(branch) || missing(instance)) {
    stop('Please, specify branch and instance for ', table)
  }
  if(missing(folder) || missing(table)) {
    stop('Please, specify branch and instance for ', table)
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
                            .verb     = 'update')

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
