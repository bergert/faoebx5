#' @title Get EBX5 Connection details
#'
#' @description This is a utility function to return connection details
#' required for using the EBX5 SOAP web-API. This function is not intended for
#' users, it is exported beacuse it is useful for test purposes.
#'
#' @seealso \code{\link{SetupEBXConnection}}.
#'
#' @return Returns a \code{\link[data.table]{data.table}} with the connection details
#'
#' @importFrom yaml yaml.load_file
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetEBXConnection <- function() {

  if (!file.exists('ebx5_connection.yaml')) {
    stop('the file [ebx5_connection.yaml] cannot be found')
  }

  connection <- yaml.load_file('ebx5_connection.yaml')

  if (length(connection) == 5) {
    return(connection)
  }

  stop('the EBX conncetionn must be setup using SetupEBXConnection()')

}
