#' @title Remove EBX Connection from the secure store
#'
#' @param username username, a character.
#'
#' @description This function aimed to remove your EBX credentials previously
#' created using the function \code{\link{SetupEBXConnection}}.
#'
#' @details When you run the function \code{RemoveEBXConnection()} will open
#' a box to type your password twice. This function tries to guess your username
#' using the variable stored in the system, Sys.getenv('USERNAME'). Otherwise,
#' you can provide your username by the argument \code{username}.
#'
#' @return Status 0 (zero) ok.
#'
#' @importFrom keyring key_get keyring_list key_set key_list
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
#' @author Luís G. Silva e Silva, \email{luis.silvaesilva@fao.org}
RemoveEBXConnection <- function(username) {

  if(missing(username)) {
    stop('EBX SOAP credentials are required')
  }

  keys_ebx_lock <- keyring::key_list(service = "EBX_SECRET", keyring = "EBX")
  keys_ebx_unlocked <- keyring::key_list(service = "EBX_SECRET")

  if (nrow(keyring::key_list(service = "EBX_SECRET", keyring = "EBX")) > 0) {

    keyring::key_delete(service = 'EBX_SECRET',keyring = 'EBX', username=username)

    message('EBX connection setup has been REMOVED successfully')

    return(c('OK' = 0))
  }

  if (nrow(keyring::key_list(service = "EBX_SECRET")) > 0) {

    keyring::key_delete(service = 'EBX_SECRET', username=username)

    message('EBX connection setup has been REMOVED successfully')

    return(c('OK' = 0))
  }
}
