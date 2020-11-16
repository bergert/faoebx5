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
soap_request <- function(.user,
                         .secret,
                         .verb,
                         .table,
                         .branch,
                         .instance) {

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

#' @title Builds the XML body with data
#'
#' @description Builds the XML body which containd data.
#'
#' @param .user username
#' @param .secret user password
#' @param .branch branch name
#' @param .instance intance name
#' @param .folder folder name
#' @param .folder2 optinal second level folder
#' @param .table the table name
#' @param .verb SOAP action verb
#'
#' @return character
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
#'
body_request_data <- function(.user,
                              .secret,
                              .branch,
                              .instance,
                              .folder,
                              .folder2,
                              .table,
                              .verb) {

  if (is.na(.folder2) || .folder2=='') {
     folder <- sprintf('<%s></%s>', .folder, .folder);
  }
  else {
    folder <- sprintf('<%s>
                       <%s></%s>
                      </%s>',
                      .folder,
                      .folder2, .folder2,
                      .folder);
  }
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
                  <data>
                  <root>
                  %s
                  </root>
                  </data>
                  </urn:%s_%s>
                  </soapenv:Body>
                  </soapenv:Envelope>',
                  .user,
                  .secret,
                  .verb, .table,
                  .branch,
                  .instance,
                  folder,
                  .verb, .table)

  return(body)
}

#' @title Convert to XML; internal
#'
#' @description Create XML for soap request
#'
#' @param .data data for request
#' @param .table table name in EBX
#' @return XML character
#'
#' @importFrom XML newXMLNode
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
data_convert_xml <- function(.data, .table = .table) {

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
