#' @title Soap header; internal
#'
#' @description Create the XML header
#'
#' @param soap_verb EBX5 SOAP verb
#'
#' @return XML character with the header fields
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
header_fields <- function(soap_verb) {

  if(missing(soap_verb)) {
    stop('<soap_verb> is required')
  }

  header <- c(Accept = "text/xml",
              Accept = "multipart/*",
              `Content-Type` = "text/xml;charset=UTF-8",
              SOAPAction = sprintf('"%s"', soap_verb))
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
soap_request_read <- function(.user,
                         .secret,
                         .verb,
                         .table,
                         .branch,
                         .instance) {

  body <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
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
soap_request_update <- function(.user,
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
  body <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
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

#' @title Builds the XML body for a delete request
#'
#' @description Builds the XML body which containd data.
#'
#' @param .user username
#' @param .secret user password
#' @param .branch branch name
#' @param .instance intance name
#' @param .table the table name
#' @param .predicate the EBX predicate is an XPath expression
#'
#' @return character
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
#'
soap_request_delete <- function(.user,
                              .secret,
                              .branch,
                              .instance,
                              .table,
                              .predicate) {

  body <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
                  <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:ebx-schemas:dataservices_1.0" xmlns:sec="http://schemas.xmlsoap.org/ws/2002/04/secext">
                  <soapenv:Header>
                  <sec:Security>
                  <UsernameToken>
                  <Username>%s</Username>
                  <Password>%s</Password>
                  </UsernameToken>
                  </sec:Security>
                  </soapenv:Header>
                  <soapenv:Body>
                  <urn:delete_%s>
                  <branch>%s</branch>
                  <instance>%s</instance>
                  <predicate>%s</predicate>
                  </urn:delete_%s>
                  </soapenv:Body>
                  </soapenv:Envelope>',
                  .user,
                  .secret,
                  .table,
                  .branch,
                  .instance,
                  .predicate,
                  .table)

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
