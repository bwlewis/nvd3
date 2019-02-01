#' Basic interactive plots using nvd3.js and htmlwidgets.
#'
#' Basic interactive plots using nvd3.js and htmlwidgets.
#'
#' @name nvd3-package
#' @references
#' \url{http://nvd3.org}
#' @aliases nvd3
#' @examples
#' \dontrun{
#' library("shiny")
#'
#' # See also help for nvd3.js
#' }
#'
#' @docType package
NULL


#' Shiny bindings for nvd3 widgets
#'
#' Output and render functions for using nvd3 widgets within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param expr An expression that generates nvd3 graphics.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @importFrom htmlwidgets shinyWidgetOutput
#' @importFrom htmlwidgets shinyRenderWidget
#'
#' @name nvd3-shiny
NULL
