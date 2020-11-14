#' @title Soap header; internal
#'
#' @description Create the XML header
#'
#' @param soap_url EBX5 SOAP URL to API requests
#'
#' @return XML character with the header fields
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

#' @title Insert XML; internal
#'
#' @description Create XML for soap insert request
#'
#' @param .data data for request
#' @param .table table name in EBX
#' @return XML character
#'
#' @importFrom XML newXMLNode
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
cl_data_insert_xml <- function(.data, .table = 'EBXCodelist') {

  features <- names(.data)
  out_list <- list()

  for(j in 1:nrow(.data)) {

    out_list[[j]] <- newXMLNode(name = .table)

    for(i in 1:ncol(.data)) {
      newXMLNode(name = features[i], .data[j, i], parent = out_list[[j]])
    }
  }

  return(out_list)
}

#' @title Builds the XML body to insert data; internal
#'
#' @description Builds the XML body to insert data
#'
#' @param .user EBX5 user name
#' @param .secret name secret
#' @param .verb action (select, update, insert, delete)
#' @param .table the code-list name
#' @param .branch dataset branch
#' @param .instance dataset instance
#'
#' @return character XML
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
soap_request <- function(.user, .secret, .verb, .table, .branch, .instance) {

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
                  .verb, .table,
                  .branch,
                  .instance,
                  .verb, .table)

  return(body)
}

#' @title get table; internal
#'
#' @description Reads a table from EBX
#'
#' @param connection connection details
#' @param branch table branch
#' @param instance table instance
#' @param table_name table name
#'
#' @return character XML
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
getEBX_Table <- function(connection, branch, instance, table_name) {

  if (is.na(branch) | is.na(instance) | is.na(table_name)) {
    stop('Cannot find branch,instance for ', table_name)
  }


  ##-- SOAP: Header ----
  headerFields <- header_fields(connection$ebx_soap_url)

  ##-- Body: request ----
  body <- soap_request    (.user     = connection$username,
                           .secret   = connection$password,
                           .verb     = 'select',
                           .table    = table_name,
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
  df  <- xmlToDataFrame(nodes = getNodeSet(doc, paste0("//", table_name)), stringsAsFactors = F)
  attr(df, ".internal.selfref") <- NULL

  return(df)
}

#' @title update; internal
#'
#' @description Updates a table
#'
#' @param connection connection details
#' @param branch table branch
#' @param instance table instance
#' @param table_name table name
#' @param verb what todo (select, insert, update)
#'
#' @return TRUE if update was done
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
updateTable <- function(connection, branch, instance, folder, data, table, verb) {

  if (is.na(branch) | is.na(instance) | is.na(table)) {
    stop('Cannot find branch,instance for ', table)
  }


  ##-- SOAP: Header ----
  headerFields <- header_fields(connection$ebx_soap_url)

  ##-- Body: request ----
  body <- soap_request    (.user     = connection$username,
                           .secret   = connection$password,
                           .verb     = verb,
                           .table    = table,
                           .branch   = branch,
                           .instance = instance)
  ##-- Building XML object----
  out_list <- cl_data_insert_xml(.data = data, .table = table)

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

#' @title update; internal
#'
#' @description Updates a table
#'
#' @param data user data
#' @param sdmx_name SDMX table name
#' @param verb what todo (select, insert, update)
#' @param instance table instance
#' @param isCodeList TRUE uses ebx5.cl_datal FALSE uses ebx5.gr_data
#'
#' @return TRUE if update was done
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
update <- function(data, sdmx_name, verb, isCodeList = TRUE) {

  if(missing(data) || missing(sdmx_name)) {
    stop('Please, provide the data and code list name.')
  }

  #-- connection details ----
  connection <- GetEBXConnection()

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
    table    <- as.character(ebx5.cl_data$Name[ebx5.cl_data$Acronym == sdmx_name])
  }
  else {
    if(!exists("ebx5.gr_data")) {
      ebx5.gr_data <<- GetEBXGroups()
    }
    #-- resolve the acutal location using metadata ----
    branch   <- as.character(ebx5.gr_data$Branch[ebx5.cl_data$Acronym == sdmx_name])
    instance <- as.character(ebx5.gr_data$Instance[ebx5.cl_data$Acronym == sdmx_name])
    folder   <- as.character(ebx5.gr_data$Folder[ebx5.cl_data$Acronym == sdmx_name])
    table    <- as.character(ebx5.gr_data$Name[ebx5.cl_data$Acronym == sdmx_name])
  }


  if (is.na(branch) | is.na(instance) | is.na(table) | is.na(folder)) {
    stop('Cannot find branch,instance,folder for ', table)
  }

  #-- update to EBX5
  return ( updateTable(connection, branch, instance, folder, data, table, verb=verb))
}
