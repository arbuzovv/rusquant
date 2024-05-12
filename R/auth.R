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
auth <- function(src='Moex',login,password) {
  if(src=='Moex')
  {
    if (nzchar(login)) {
      Sys.setenv(MOEX_DATASHOP_LOGIN = login)
      Sys.setenv(MOEX_DATASHOP_PASSWORD = password)
      url_auth = 'https://passport.moex.com/authenticate'
      headers = c('Authorization' =  paste0('Basic ',base64encode(charToRaw(paste(login,password,sep = ':')))))
      auth_response = NULL
      tryCatch(
        {
          auth_response <- GET(url_auth,add_headers(headers))
          if(auth_response$status_code==200)
          {
            cookie_value = content(auth_response, "text", encoding = "UTF-8")
            Sys.setenv(MOEX_DATASHOP_COOKIE = cookie_value)
            message("succes authentication to ISS Moex using login/password")
          }
        },
        #if an error occurs, tell me the error
        error=function(e) {
          message('Server of MOEX not response - try later')
          #print(e)
        },
        #if a warning occurs, tell me the warning
        warning=function(w) {
          message('Check your internet connection')
        }
      )
    } else {
      stop("Invalid login. Login must be a non-empty string.")
    }
  }
  invisible(NULL)
}




