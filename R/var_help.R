#' Get descriptions of variables
#'
#' @param dataName name of the file that has the 1-line text description of the variables.
#' @param ... character strings naming the variables for which help is to be delivered
#' @param line_length integer specifying the character length at which
#' the output lines should broken. Use `Inf` for no breaking.
#'
#' @return a text string with the descriptions of the requested variables
#'
#' @examples
#' la_var_help("NHANES", "Age", "SmokeNow")
#'
#' @export
LA_var_help <- function(dataName, ..., line_length = 50L) {
  text <-
    if (dataName %in% names(var_help_cache)) var_help_cache[[dataName]]
    else {
      fname <- paste0("variable_documentation/", dataName, ".txt")
      source_file <- system.file(fname, package = "LittleApps")
      if (source_file == "") stop("No variable documentation file named ", fname, ".")
      text <- readLines(source_file)
      var_help_cache[[dataName]] <- text
      text
    }

    vnames = list(...)
    res <- lapply(vnames, FUN = function(nm) {text[grep(paste0("^", nm,":"), text)]})
    res <- unlist(res)

    res <- paste(res, collapse = "\n\n")
    regex <- sprintf("(.{1,%d})(\\s|$)", line_length)
    if (line_length != Inf)    res <- gsub(regex, "\\1\n", res)
    # res <- gsub('(.{1,50})(\\s|$)', '\\1\n', res)

    res

}

var_help_cache <- list()


