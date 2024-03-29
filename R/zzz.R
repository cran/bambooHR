.pkgenv <- new.env(parent=emptyenv())

.get_config_file <- function() {
  getOption("bambooHR.config_file", default="~/.bambooHR_user_config.json")
}

.onLoad <- function(libname, pkgname) {
  config <- .get_config_file()
  if (file.exists(config)) use_config(config)
}



.get_api_key <- function() {
  getOption("bambooHR.api_key",                # retrieve as option,
            if (!is.null(.pkgenv$conf))         # else try environment
              .pkgenv$conf[["key"]]           # and use it, or signal error
            else stop(paste("Neither option 'bambooHR.api_key' nor entry in",
                            "package environment found. Aborting."), call.=FALSE))
}

.get_company_name <- function() {
  getOption("bambooHR.company_name",       	# retrieve as option,
            if (!is.null(.pkgenv$conf))   	# else try environment
              .pkgenv$conf[["bambooHR.company_name"]]       # and use it, or signal error
            else stop(paste("Neither option 'bambooHR.company_name' nor entry in",
                            "package environment found. Aborting."), call.=FALSE))
}



#' Create a JSON config file
#'
#' @param apikey API key to access the bambooHR API. User will be prompted to enter one if not supplied
#' @param companyname This is the subdomain used to access BambooHR. If you access BambooHR at https://mycompany.bamboohr.com, then companyname will be "mycompany"
#' @param conffile A string giving the path where the configuration file will be written.
#' This will automatically attempt to load from the default location \code{~/.bambooHR_user_config.json} (which can be changed via a \code{bambooHR.config_file}) entry in \code{options}).
#' @param verbose whether to print path of conf file used, passed to use_config
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config_setup("secret api key", "cool company")
#' }
#'
config_setup <- function(apikey, companyname, conffile, verbose = TRUE) {

  if (missing(apikey))
    apikey <- readline("Please enter your API key: ")
  if (missing(companyname)) companyname <- NA
  if (missing(conffile)) conffile <- .get_config_file()


  reslist <- list(api_key = apikey, company_name = companyname)
  json <- jsonlite::toJSON(reslist, auto_unbox=TRUE, pretty=TRUE)

  f <- file(conffile,open = "w")
  cat(json,file = f)
  close(f)
  ## User Read only
  Sys.chmod(conffile, mode = "600")
  use_config(conffile, verbose)
  invisible(NULL)
}
