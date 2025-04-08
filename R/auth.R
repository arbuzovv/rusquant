#' Set creds for Datasource
#'
#' Sets creds for the Datasource  by storing it in an
#' environment variable for the current R session. This token will be used by
#' other functions in the package to authenticate API requests.
#'
#' @param src datasource name.
#' @param login login of datasource
#' @param password password of datasource
#' @return Invisible NULL, side-effect function setting an environment variable.
#' @examples
#' \dontrun{
#'   auth(login = "user@email.com",password = "mypassword")
#' }
#' @export
auth <- function(src='Moex',api_key) {
  if(src=='Moex')
  {

    if (nzchar(api_key)) {
      Sys.setenv(MOEX_API_KEY = api_key)
    } else {
      stop("Invalid login. Login must be a non-empty string.")
    }
  }
  invisible(NULL)
}




