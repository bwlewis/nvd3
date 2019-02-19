#' @rdname nvd3-shiny
#' @export
nvOutput <- function(outputId, width="100%", height="600px") {
    shinyWidgetOutput(outputId, "nv", width, height, package = "nvd3")
}

#' rendernv
#' @export
rendernv <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) expr <- substitute(expr) # force quoted
    shinyRenderWidget(expr, nvOutput, env, quoted = TRUE)
}
