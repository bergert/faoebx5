#' @title Setup EBX Connection
#'
#' @param meta_branch metadata branch name in EBX
#' @param meta_instance metadata instance name in EBX
#' @param ebx_soap_url SOAP webservice url
#' @param username username, a character.
#' @param password password, a character.
#' @param lock logical, default is FALSE. If it is TRUE, will store a locked file with the password,
#' and whenever trying to use the credential will ask for the password to unlock the connection setup.
#'
#' @seealso \code{\link{RemoveEBXConnection}}.
#'
#' @description This function will store the SOAP connection details in a yaml file
#'
#' @return Status 0 (zero) ok.
#'
#' @importFrom keyring key_get keyring_list key_set
#' @importFrom yaml as.yaml
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
SetupEBXConnection <- function(meta_branch, meta_instance, ebx_soap_url, username, password) {

  if(missing(meta_branch) || missing(meta_instance) || missing(ebx_soap_url)) {
     stop('EBX communication details are missing: <meta_branch>, <meta_instance>, <ebx_soap_url>')
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

