#' Set creds for Datasource
#'
#' Sets creds for the Datasource  by storing it in an
#' environment variable for the current R session. This token will be used by
#' other functions in the package to authenticate API requests.
#'
#' @param src datasource name.
#' @param api_key api_key of datasource
#' @return Invisible NULL, side-effect function setting an environment variable.
#' @examples
#' \dontrun{
#'   auth(api_key = "")
#' }
#' @export
auth <- function(src='Moex',api_key) {
  if(src=='Moex')
  {
      Sys.setenv(MOEX_DATASHOP_API_KEY = api_key)
  }
}




