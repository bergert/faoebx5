#' @title Read EBX5 Group
#'
#' @description This function aimed to read group data from EBX5 to R.
#'
#' @param sdmx_name group name which the data will be read from.  Please, see
#' the code list options after run the function \code{\link{GetEBXGroups}} in the field "Acronym".
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
#' gr <- ReadEBXGroup(sdmx_name = 'HCL_FI_COMMODITY_FAOL1_FAOL2')
#' }
#'
#' @export
#'
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
ReadEBXGroup <- function(sdmx_name) {

  if(!exists("ebx5.gr_data")) {

    ebx5.gr_data <<- GetEBXGroups()

  }

  if(missing(sdmx_name)) {
    stop('Please, provide the code list name.')
  }

  branch <- as.character(ebx5.gr_data$Branch[ebx5.gr_data$Acronym == sdmx_name])
  instance <- as.character(ebx5.gr_data$Instance[ebx5.gr_data$Acronym == sdmx_name])
  gr_name <- as.character(ebx5.gr_data$Name[ebx5.gr_data$Acronym == sdmx_name])

  if (is.na(branch) | is.na(instance) | is.na(gr_name)) {
    stop('Cannot find barnch,instance for ', gr_name)
  }

  .user <- Sys.getenv('USERNAME_EBX')

  ##-- SOAP: Header ----
  headerFields <- header_fields()

  ##-- Body: request ----
  body <- body_select_request(.user     = .user,
                              .name     = gr_name,
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
  df  <- xmlToDataFrame(nodes = getNodeSet(doc, paste0("//", gr_name)))
  df  <- data.table::data.table(df)

  return(df)
}
