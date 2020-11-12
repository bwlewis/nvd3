#' Prepare a barplot matrix for nvd3
#' @param M matrix
#' @return A custom JSON string suitable for nvd3 multibar plotting
#' @importFrom jsonlite toJSON
multibar_matrix2json = function(M)
{
  stopifnot(is.matrix(M))
  if(is.null(colnames(M))) colnames(M) = rep("", length.out=ncol(M))
  x = seq(0, nrow(M) - 1)
  l = Map(as.data.frame, apply(M, 2, function(w) list(x=x, y=as.vector(w))))
  names(l) = NULL
  ans = data.frame(key=colnames(M), values=list(cbind(l)))
  names(ans) = c("key", "values")
  toJSON(ans)
}

#' Convert a data frame into a matrix suitable for barplot input
#'
#' @param x a data frame with at least one numeric and one factor column and no more than three columns
#' @note The data frame must contain at least one factor and one numeric variable, and a total of
#'   at most three variables. In the case of two factor variables, the first one defines the x-axis
#'   barplot groups and the 2nd one the within-bar groups.
#' @return a matrix suitable for use with \code{barplot} or \code{nvbarplot}
#' @export
dftab = function(x)
{
  if(3 < ncol(x)) stop("too many variables")
  t = unlist(Map(class, x))
  fidx = t == "factor"
  if(sum(fidx) < 1) stop("need at least one categorical variable")
  if(!(any(t == "numeric" | t == "integer"))) stop("need at least one numeric variable")
  # re-factor first factor column to match order that they appear in the data frame
  # this also omits non-displayed factor levels
  i1 = which(fidx)[1]
  f = levels(x[, i1])[x[, i1]]
  f = factor(f, levels=unique(f))
  x[, i1] = f
  if(sum(fidx) == 2) {
    i2 = which(fidx)[2]
    f = levels(x[, i2])[x[, i2]]
    f = factor(f, levels=unique(f))
    x[, i2] = f
  }
  if(sum(fidx) == 1) {
    ans = as.matrix(x[, !fidx,drop=FALSE])
    rownames(ans) = x[, fidx]
    return(t(ans))
  }
  i = as.integer(x[, fidx][[2]])
  j = as.integer(x[, fidx][[1]])
  n = max(i)
  p = max(j)
  ans = matrix(0, n, p)
  ans[i + (j - 1) * n] = x[, !fidx]
  rownames(ans) = levels(x[, fidx][[2]])
  colnames(ans) = levels(x[, fidx][[1]])
  return(ans)
}


#' Render generic nvd3 plots
#'
#' Render generic nvd3 plots
#'
#' @param height Widget height
#' @param width Widget width
#' @param program JavaScript program to draw plot
#' @param elementId option widget element DOM ID
#' @param ... Optional additional parameters passed to the rendering code.
#' @return An HTML widget object
#' @importFrom htmlwidgets createWidget
#' @export
nvd3 <- function(program, height=NULL, width=NULL, elementId=NULL, ...)
{
  x <- list(program=program)
  additional_args <- list(...)
  if (length(additional_args) > 0) x <- c(x, additional_args)

  createWidget(
    name = "nv",
    x = x,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE),
    elementId = elementId,
    package = "nvd3")
}
