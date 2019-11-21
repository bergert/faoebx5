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
#' @description This function will store the SOAP connection details in a secure
#' store which is provided by Windows and macOS(keychain). On LINUX, you need to
#' install the Secret Service API ()
#' https://specifications.freedesktop.org/secret-service/latest/
#'
#' @details When you run the function \code{SetEBXCredentials()} with lock=TRUE,
#' the connection details are secured with an access password. A box to type the
#' password for unlokcing the connection deatils will appear.
#'
#' @return Status 0 (zero) ok.
#'
#' @importFrom keyring key_get keyring_list key_set
#' @importFrom yaml write_yaml
#' @export
#'
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luis G. Silva e Silva, \email{luis.silvaesilva@fao.org}
SetupEBXConnection <- function(meta_branch, meta_instance, ebx_soap_url, username, password, lock = FALSE) {

  if(missing(meta_branch) || missing(meta_instance) || missing(ebx_soap_url)) {
     stop('EBX communication details are missing: <meta_branch>, <meta_instance>, <ebx_soap_url>')
  }

  if(missing(username) || missing(password)) {
    stop('EBX SOAP credentials are required')
  }

  connection <- data.frame(username=username,
                           meta_branch=meta_branch,
                           meta_instance=meta_instance,
                           ebx_soap_url=ebx_soap_url)
  write_yaml(connection, 'ebx5_connection.yaml', fileEncoding = "UTF-8")

  if(lock) {

    message("Enter password to lock EBX credentials")
    keyring::keyring_create(keyring = "EBX")
    keyring::keyring_lock("EBX")

    # ebx_key_list <- keyring::key_list(service = "EBX_SECRET", "EBX")
    keyring::key_set_with_value(service = "EBX_SECRET",
                                username = username,
                                password = password,
                                keyring = "EBX")

    message('EBX credentials have been SET with success.')
    return(c('OK' = 0))

  } else {

    keyring::key_set_with_value(service = "EBX_SECRET",
                                username = username,
                                password = password)

    message('EBX credentials have been SET with success.')
    return(c('OK' = 0))

  }
}

