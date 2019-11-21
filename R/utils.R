#' @title Header Fields
#'
#' @description Builds the header fields
#'
#' @param soap_action EBX5 SOAP URL to API requests
#'
#' @return character with the header fields
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
header_fields <- function(soap_url) {

  if(missing(soap_url)) {
    stop('<soap_url> is required')
  }

  header <- c(Accept = "text/xml",
              Accept = "multipart/*",
              `Content-Type` = "text/xml; charset=utf-8",
              SOAPAction = soap_url)

  return(header)
}

#' @title XXX
#'
#' @description XXX
#'
#' @param .data XXX
#' @param .cl_name code list name which the data will be read from. Please, see
#' the code list options by running the function \code{\link{GetEBXCodeLists}}
#' @return XXX
#'
#' @importFrom XML newXMLNode
#'
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
cl_data_insert_xml <- function(.data,
                               .cl_name = 'EBXCodelist') {

  features <- names(.data)
  out_list <- list()

  for(j in 1:nrow(.data)) {

    out_list[[j]] <- newXMLNode(name = .cl_name)

    for(i in 1:ncol(.data)) {
      newXMLNode(name = features[i], .data[j, i], parent = out_list[[j]])
    }
  }

  return(out_list)
}


#' @title Builds the XML body to insert data
#'
#' @description Builds the XML body to insert data
#'
#' @param .type data source, EBXCodelist or EBXGroup
#'
#' @return character
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
soap_request <- function(.user, .secret, .verb, .type, .branch, .instance) {

  body <- sprintf('<?xml version="1.0" encoding="utf-8"?>
                  <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:sec="http://schemas.xmlsoap.org/ws/2002/04/secext" xmlns:urn="urn:ebx-schemas:dataservices_1.0">
                  <soapenv:Header>
                  <sec:Security>
                  <UsernameToken>
                  <Username>%s</Username>
                  <Password>%s</Password>
                  </UsernameToken>
                  </sec:Security>
                  </soapenv:Header>
                  <soapenv:Body>
                  <urn:%s_%s>
                  <branch>%s</branch>
                  <instance>%s</instance>
                  </urn:%s_%s>
                  </soapenv:Body>
                  </soapenv:Envelope>',
                  .user,
                  .secret,
                  .verb, .type,
                  .branch,
                  .instance,
                  .verb, .type)

  return(body)
}

getCodeList <- function(connection, branch, instance, cl_name) {

  if (is.na(branch) | is.na(instance) | is.na(cl_name)) {
    stop('Cannot find branch,instance for ', cl_name)
  }


  ##-- SOAP: Header ----
  headerFields <- header_fields(connection$ebx_soap_url)

  ##-- Body: request ----
  body <- soap_request    (.user     = connection$username,
                           .secret   = connection$secret,
                           .verb     = 'select',
                           .type     = cl_name,
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
  df  <- xmlToDataFrame(nodes = getNodeSet(doc, paste0("//", cl_name)), stringsAsFactors = F)
  attr(df, ".internal.selfref") <- NULL

  return(df)
}

updateCodeList <- function(connection, branch, instance, folder, data, cl_name, verb) {

  if (is.na(branch) | is.na(instance) | is.na(cl_name)) {
    stop('Cannot find branch,instance for ', cl_name)
  }


  ##-- SOAP: Header ----
  headerFields <- header_fields(connection$ebx_soap_url)

  ##-- Body: request ----
  body <- soap_request    (.user     = connection$username,
                           .secret   = connection$secret,
                           .verb     = verb,
                           .type     = cl_name,
                           .branch   = branch,
                           .instance = instance)
  ##-- Building XML object----
  out_list <- cl_data_insert_xml(.data = data, .cl_name = cl_name)

  body_xml <- xmlParse(body)
  metadata_xml <- getNodeSet(body_xml, paste0("//", folder))
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


update <- function(data, sdmx_name, verb, isCodeList = TRUE) {

  if(missing(data) || missing(sdmx_name)) {
    stop('Please, provide the data and code list name.')
  }

  #-- connection details ----
  connection <- GetConnection()

  #-- read metadata; if not already loaded
  if (isCodeList) {
    if(!exists("ebx5.cl_data")) {
      ebx5.cl_data <<- GetEBXCodeLists(connection)
    }
    if (length(ebx5.cl_data$Name[ebx5.cl_data$Acronym == sdmx_name]) == 0) {
      stop('Cannot find a codelist with acronym=<', sdmx_name, '> defined in EBX metadata')
    }
    #-- resolve the acutal location using metadata ----
    branch   <- as.character(ebx5.cl_data$Branch[ebx5.cl_data$Acronym == sdmx_name])
    instance <- as.character(ebx5.cl_data$Instance[ebx5.cl_data$Acronym == sdmx_name])
    folder   <- as.character(ebx5.cl_data$Folder[ebx5.cl_data$Acronym == sdmx_name])
    cl_name  <- as.character(ebx5.cl_data$Name[ebx5.cl_data$Acronym == sdmx_name])
  }
  else {
    if(!exists("ebx5.gr_data")) {
      ebx5.gr_data <<- GetEBXGroups()
    }
    #-- resolve the acutal location using metadata ----
    branch   <- as.character(ebx5.gr_data$Branch[ebx5.cl_data$Acronym == sdmx_name])
    instance <- as.character(ebx5.gr_data$Instance[ebx5.cl_data$Acronym == sdmx_name])
    folder   <- as.character(ebx5.gr_data$Folder[ebx5.cl_data$Acronym == sdmx_name])
    cl_name  <- as.character(ebx5.gr_data$Name[ebx5.cl_data$Acronym == sdmx_name])
  }


  if (is.na(branch) | is.na(instance) | is.na(cl_name) | is.na(folder)) {
    stop('Cannot find branch,instance,folder for ', sdmx_name)
  }

  #-- update to EBX5
  return ( updateCodeList(connection, branch, instance, folder, data, cl_name, verb=verb))
}
