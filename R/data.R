#' W3C Character References
#'
#' A dataframe containing characters, their hexadecimal, decimal and HTML named entity codes and other attributes.
#'
#' @format A dataframe with 1,448 rows and 8 columns:
#' \describe{
#'   \item{names}{name of symbol and description}
#'   \item{character}{character symbol}
#'   \item{hex}{hexadecimal code for symbol}
#'   \item{dec}{decimal code for symbol}
#'   \item{ent}{HTML named entity code for symbol}
#'   \item{block}{Unicode block for symbol}
#'   \item{category}{Unicode category for symbol}
#'   \item{charset}{character set that symbol belongs to}
#' }
#'
#' @source \url{https://dev.w3.org/html5/html-author/charref}
"w3charref"
