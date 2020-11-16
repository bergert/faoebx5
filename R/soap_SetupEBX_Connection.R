#' @title Setup EBX Connection
#'
#' @param meta_branch metadata branch name in EBX, used for faostatR library (optional)
#' @param meta_instance metadata instance name in EBX, used for faostatR library (optional)
#' @param ebx_soap_url SOAP webservice url
#' @param username username, a character.
#' @param password password, a character.
#'
#' @seealso \code{\link{GetEBXConnection}}.
#'
#' @description This function will store the SOAP connection details in a yaml file
#'
#' @return Status 0 (zero) ok.
#'
#' @details In case of an shiny app, run the \code{\link{SetupEBXConnection}} from your Rstudio.
#' The conncetion details are persisted to a file [ebx5_connecion.yml] which you can then embed into the shiny app.
#' The library faoebx5 uses \code{\link{GetEBXConnection}}, in order to obtain the connection details from the yaml file.
#' Obviously, the yaml file is excluded, not part of the GIT repository faoebx5.
#'
#' @importFrom yaml as.yaml
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
SetupEBXConnection <- function(
      meta_branch = 'Fishery',
      meta_instance = 'Fishery',
      ebx_soap_url,
      username,
      password) {

  if(missing(ebx_soap_url)) {
     stop('EBX communication details are missing: <ebx_soap_url>')
  }

  if(missing(username) || missing(password)) {
    stop('EBX SOAP credentials are required')
  }

  connection <- list(username=username,
                           password=password,
                           meta_branch=meta_branch,
                           meta_instance=meta_instance,
                           ebx_soap_url=ebx_soap_url)
  yaml_str <- as.yaml(connection)

  filehandle <- file("ebx5_connection.yaml",encoding="UTF-8")
  write(yaml_str, file=filehandle)
  close(filehandle)

  message('EBX credentials have been WRITTEN to [ebx5_connection.yaml] successfully.')
  return(c('OK' = 0))
}

