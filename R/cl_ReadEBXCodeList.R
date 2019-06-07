#' @title Read EBX5 Code List
#'
#' @description This function aimed to read code list data from EBX5 to R.
#'
#' @param sdmx_name code list name which the data will be read from. Please, see
#' the code list options by running the function \code{\link{GetEBXCodeLists}} in the field "Acronym".
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
ReadEBXCodeList <- function(sdmx_name) {

  if(!exists("ebx5.cl_data")) {

    ebx5.cl_data <<- GetEBXCodeLists()

  }

  if(missing(sdmx_name)) {
    stop('Please, provide the code list name.')
  }

  branch <- as.character(ebx5.cl_data$Branch[ebx5.cl_data$Acronym == sdmx_name])
  instance <- as.character(ebx5.cl_data$Instance[ebx5.cl_data$Acronym == sdmx_name])
  cl_name <- as.character(ebx5.cl_data$Name[ebx5.cl_data$Acronym == sdmx_name])

  if (is.na(branch) | is.na(instance) | is.na(cl_name)) {
    stop('Cannot find barnch,instance for ', cl_name)
  }

  .user <- Sys.getenv('USERNAME_EBX')

  ##-- SOAP: Header ----
  headerFields <- header_fields()

  ##-- Body: request ----
  body <- body_select_request(.user     = .user,
                              .name     = cl_name,
                              .branch   = branch,
                              .instance = instance)

  ##-- API request ----
  reader <- basicTextGatherer()
  header <- basicTextGatherer()

  curlPerform(url = headerFields["SOAPAction"],
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
  df  <- xmlToDataFrame(nodes = getNodeSet(doc, paste0("//", cl_name)))
  df  <- data.table::data.table(df)

  return(df)
}
