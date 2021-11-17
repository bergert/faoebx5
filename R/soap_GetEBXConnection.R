#' @title Get EBX5 Connection details
#'
#' @description This is a utility function to return connection details
#' required for using the EBX5 SOAP web-API. This function is not intended for
#' users, it is exported beacuse it is useful for test purposes.
#' The connection can be setup in two ways: 1) ebx5_connection.yaml file created by
#' \code{\link{SetupEBXConnection}} or using environment variables (EBX5_URL,
#' EBX5_USERID, EBX5_SECRET, BRANCH, INSTANCE)
#'
#' @seealso \code{\link{SetupEBXConnection}}.
#'
#' @return Returns a \code{\link[data.table]{data.table}} with the connection details
#'
#' @details The connection to EBX5 is required for use of this library.
#' For use with a shiny app, the best way is to setup the environment variables in the Rstudio
#' connect app settings panel. If this is not possible (hqlprsws1 server), or for local use
#' you cerate the YAML file. With the yaml file present (user PC), or the environment variables
#' your shiny app will work without code change.
#'
#' @importFrom yaml yaml.load_file
#'
#' @export
#'
#' @author Thomas Berger, \email{thomas.berger@fao.org}
GetEBXConnection <- function() {

  x <- Sys.getenv(c("EBX5_URL", "EBX5_USERID", "EBX5_SECRET", "BRANCH", "INSTANCE"))
  if (x["EBX5_URL"] != "" && x["EBX5_USERID"] != "" && x["EBX5_SECRET"] != "" && x["BRANCH"] != "" && x["INSTANCE"] != "") {
    connection <- list(username=unname(x["EBX5_USERID"]), password=unname(x["EBX5_SECRET"]),
                       meta_branch=unname(x["BRANCH"]), meta_instance=unname(x["INSTANCE"]), ebx_soap_url=unname(x["EBX5_URL"]))
    return(connection)
  }

  if (!file.exists('ebx5_connection.yaml')) {
    stop('the file [ebx5_connection.yaml] cannot be found')
  }

  connection <- yaml.load_file('ebx5_connection.yaml')
  if (length(connection) == 5) {
    return(connection)
  }

  stop('the EBX connectionn must be setup using environment variables, or using SetupEBXConnection()')

}
