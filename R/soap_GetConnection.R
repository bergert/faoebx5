#' @title Get EBX5 Connection details
#'
#' @description This is a utility function to return connection details
#' required for using the EBX5 SOAP web-API. This function is not intended for
#' users, therefore it is not exported.
#'
#' @seealso \code{\link{SetupEBXConnection}}.
#' @seealso \code{\link{RemoveEBXConnection}}.
#'
#' @return Returns a \code{\link[data.table]{data.table}} with the connection details
#'
#' @importFrom yaml read_yaml
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetConnection <- function() {

  connection <- read_yaml('ebx5_connection.yaml', fileEncoding = "UTF-8")

  users_ebx_lock <- keyring::key_list(service = "EBX_SECRET", keyring = "EBX")
  if (nrow(users_ebx_lock) > 0) {
    return(c(connection,
             secret=keyring::key_get("EBX_SECRET", username = username, keyring = "EBX")))
  }

  users_ebx_unlocked <- keyring::key_list(service = "EBX_SECRET")
  if (nrow(users_ebx_unlocked) > 0) {
    return(c(connection,
             secret=keyring::key_get("EBX_SECRET", username = connection$username)))
  }

  stop('the EBX conncetionn must be setup using SetupEBXConnection()')

}
