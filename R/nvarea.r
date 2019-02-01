#' nv stacked area plot
#' 
#' @param x a data frame with at least two columns whose first column contains
#'   numeric x-axis values and remaining columns contain stacked area y-axis values.
#' @param nxticks number of x tick marks, set to 0 for none.
#' @param xticklabels either a character-valued function of a single numeric value
#'  that converts x-axis values to text, or a vector of labels as long as \code{nxticks}.
#' @param ytickformat format numeric tick labels using a d3.format string (d3.js)
#' @param interpolate d3.js plot interpolation option, either 'step', 'linear', or 'basis.'
#' @param controls (logical), if \code{TRUE} then show optional interactive plot format controls
#' @param xlim an optional numeric vector of two values of lower and upper x-axis limits
#' @param ylim an optional numeric vector of two values of lower and upper y-axis limits
#' @param tooltip an optional character vector with as many entries as \code{x} has rows, the associated entry is displayed when the user's mouse hovers over the corresponding x-xaxis value (assumes that \code{x} has distinct x-axis values). The character values may be formatted with HTML formatting.
#' @param options optional additional JavaScript options passed directly to nvd3.js.
#' @return
#' An htmlwidget object that is displayed using the object's show or print method.
#' (If you don't see your widget plot, try printing it with the \code{print} function.)
#' @importFrom jsonlite toJSON
#' @examples
#' data("stackedArea", package="nvd3")
#' head(stackedArea)  # note POSIX date format in 1st column
#' nvareaplot(stackedArea)
#'
#' # Now plot with a better x-axis format:
#' nvareaplot(stackedArea, xticklabels=function(x)
#'              format(as.POSIXct(x, origin="1970-1-1"), "%Y-%m-%d"))
#'
#' @export
nvareaplot = function(x, nxticks=10, xticklabels=NULL, ytickformat=",.1f", interpolate=c("step", "linear", "basis", "step-before", "step-after", "bundle", "cardinal", "monotone"),
                      controls=FALSE, xlim, ylim, tooltip, options)
{
  if(nrow(x) != length(unique(x[[1]]))) warning("non-unique entries in the x-axis values from first column of x (consider adding columns to represent the extra values)")
  x = x[order(x[[1]]),]
  interpolate = match.arg(interpolate)
  if(missing(ylim)) ylim = NA
#  if(missing(ylim)) {
#    ylim = Reduce(function(x, y) c(min(x[1], y[1]), x[2] + y[2]), Map(range, x[, 2:ncol(x), drop=FALSE]))
#    ylim[1] = ylim[1] - 0.1 * abs(ylim[1])
#    ylim[2] = ylim[2] + 0.1 * abs(ylim[2])
#  }
  if(missing(xlim)) xlim = NA
  if(missing(options)) options = ""
  if(!is.data.frame(x) || ncol(x) < 2) stop("invalid x argument")
  if(controls) {
    controls = "true"
  } else {
    controls = "false"
  }
  x[[1]] = as.numeric(x[[1]]) # XXX check for failed conversion
  if(nxticks < 2) nxticks = 2 # XXX FIX
  i = seq(from=min(x[[1]]), to=max(x[[1]]), length.out=nxticks)
  xticks = as.list(as.character(i))
  if(!is.null(xticklabels))
  {
    if(is.function(xticklabels)) xticks = Map(xticklabels, i)
    else xticks = as.list(xticklabels)
  }
  names(xticks) =  as.character(i)
  xtick_index = i
  values = Map(function(y) {ans = cbind(x[[1]], y); dimnames(ans)=c(); ans}, x[,-1,drop=FALSE])
  names(values) = c()
  a = vector(mode="list", length=2)
  a[[1]] = names(x)[-1]
  a[[2]] = values
  names(a) = c("key", "values")
  class(a) = "data.frame"
  attr(a, "row.names") = as.character(seq(length(a[[1]])))
  if(missing(tooltip)) {
    tooltip = "null"
  } else {
    if(length(tooltip) != nrow(x)) stop("tooltip must be a vector as long as the number of rows of x") 
    if(!is.list(tooltip)) tooltip = as.list(tooltip)
    names(tooltip) = as.character(x[[1]])
    tooltip = toJSON(tooltip, auto_unbox=TRUE)
  }
  data = sprintf("var rdata=%s;\nvar xticks=%s;\nvar xtick_index=%s;\nvar xlim=%s\nvar ylim=%s;\nvar tooltip=%s;\n",
     toJSON(a), toJSON(xticks, auto_unbox=TRUE), toJSON(xtick_index), toJSON(xlim, auto_unbox=TRUE),
     toJSON(ylim, auto_unbox=TRUE), tooltip)
  chart = sprintf("
var chart = nv.models.stackedAreaChart()
                  .margin({right: 100})
                  .interpolate('%s')
                  .x(function(d) { return d[0] })
                  .y(function(d) { return d[1] })
                  .useInteractiveGuideline(true)
                  .rightAlignYAxis(true)
                  .showControls(%s)
                  .clipEdge(true);", interpolate, controls)
  program = sprintf("
%s
nv.addGraph(function() {
  %s%s
  chart.xAxis.tickValues(xtick_index).ticks(xtick_index.length).tickFormat(function (d) { return xticks[d];});
  if(ylim) chart.yDomain(ylim);
  if(xlim) chart.xDomain(xlim);
  chart.yAxis.tickFormat(d3.format('%s'));
  if(tooltip)  chart.interactiveLayer.tooltip.contentGenerator(function (d) { return tooltip[d.value]; });
  d3.select(_this.svg)
      .datum(rdata)
      .call(chart);
  nv.utils.windowResize(chart.update);
  return chart;
});", data, chart, options, ytickformat)

  nvd3(program)
}
