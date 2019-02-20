#' nv_coords
#' Define a scatter or line plot object for use with nvmulti plots.
#'
#' @param x A vector of x-axis coordinates
#' @param y A vector of y-axis coordinates
#' @param key a character label for the scatter plot
#' @param type the type of plot, either "scatter" or "line"
#' @param yAxis which y-Axis to use
#' @export
nv_coords = function(x, y, key="data", yAxis=1, type=c("scatter", "line"))
{
  type = match.arg(type)
  if(length(x) != length(y)) stop("x and y must be the same length")
  values = data.frame(x=as.numeric(x), y=as.numeric(y), series=rep(0L, length(y)))
  structure(list(list(key=key, values=values, type=type, yAxis=yAxis, originalKey=key)), class="nv_coords")
}

#' nv_bar
#' Define a bar plot object for use with nvmulti plots.
#' @note When \code{x} is a matrix, its row names will be used to identify
#'   bars. However, column names are not yet supported to identify grouped
#'   sets of bars which instead will be numbered in order.
#' @param x a vector of heights or a matrix where each row
#'        represents a vector of heights
#' @param yAxis which y-Axis to use
#' @export
nv_bar = function(x, yAxis=1)
{
  if(!is.matrix(x)) x = rbind(x)
  i = seq(ncol(x)) - 1
  k = seq(nrow(x))
# XXX series name (colnames) not working yet.
  structure(Map(function(k) {
    key = rownames(x)[k]
    if(is.null(key)) key = paste(k)
    series = colnames(x)
    if(is.null(series)) series = paste(seq(ncol(x)))
    values = data.frame(x=i, y=x[k, ], series=series, key=rep(key, length(i)))
    list(key=key, values=values, type="bar", yAxis=yAxis, originalKey=key)
  }, seq(nrow(x))), class="nv_bar")
}

#' nv_area
#'
#' @param x a numeric matrix with at least 2 columns whose first column contains x-coordinate values
#'        and remaining columns series y-coordinate values
#' @param yAxis which y-Axis to use (1 or 2)
#' @export
nv_area = function(x, yAxis=1)
{
  if(!is.matrix(x)) x = cbind(x)
  if(!is.numeric(x)) stop("x must be a numeric matrix")
  if(ncol(x) > 1) {
    i = x[, 1]
    x = x[, -1, drop=FALSE]
  } else i = seq(nrow(x)) - 1    # i = x-axis coords
  cs = cbind(0,t(apply(x,1,cumsum))[,-ncol(x)])
  structure(Map(function(k) {
    key = colnames(x)[k]
    if(is.null(key)) key = paste(k)
    series = key
    display = apply(cbind(x[,k], cs[,k]), 1, function(z) list(y=z[1],y0=z[2]))
    values = data.frame(index = seq(nrow(x))-1, x=i, y=x[,k], series=series, seriesIndex=k-1)
    values$display = display
    list(key=key, values=values, type="area", yAxis=yAxis, originalKey=key, seriesIndex=k-1)
  }, seq(ncol(x))), class="nv_area")
}



#' nvmulti
#' Overylay scatter, line, bar, and stacked area plots on one or two vertical axes.
#'
#' @param ... \code{nv_coords}, \code{nv_bar}, and/or \code{nv_area} objects to plot.
#' @param options advanced JavaScript options
#' @param tooltip set to TRUE to display mouse hover point summary tooltip
#' @param guideline set to TRUE to display mouse hover summary of all series
#' @param interpolate d3.js plot interpolation option
#' @param xlim an optional numeric vector of two values of lower and upper x-axis limits
#' @param ylim1 an optional numeric vector of two values of lower and upper left y-axis limits
#' @param ylim2 an optional numeric vector of two values of lower and upper right y-axis limits
#' @param col optional colors, one for each series
#' @param xformat function that converts numeric x-axis values to text
#' @param xticks optional numeric vector of x tick locations
#' @importFrom jsonlite toJSON
#' @examples
#' area = nv_area(as.matrix(cbind(seq(nrow(iris)), iris[, -5])))
#' total = nv_coords(x=seq(nrow(iris)), y=apply(iris[, -5], 1, sum), type="line", key="TOTAL")
#' print(nvmulti(area, total, col=c("#8dd3c7", "#ffffb3", "#fb8072", "#80b1d3", "#000000")))
#' @export
nvmulti = function(..., tooltip=FALSE, guideline=TRUE, 
interpolate=c("linear", "step", "basis", "step-before", "step-after", "bundle", "cardinal", "monotone"),
xformat = function(x) sprintf("%d", x),
xticks, col, xlim, ylim1, ylim2, options="")
{
  interpolate = match.arg(interpolate)
  tooltip = ifelse(tooltip, "true", "false")
  guideline = ifelse(guideline, "true", "false")
  objects = Map(unclass, Reduce(c, list(...)))
  c11 = c("#8dd3c7", "#ffffb3", "#fb8072", "#80b1d3", "#fdb462", "#b3de69",
          "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f")
  if(missing(col)) col = rep(c11, length.out=length(objects))
  # make sure x-coordinates are numeric
  for(x in objects) x$values$x = as.numeric(x$values$x)
  xvals = unlist(Map(function(x) x$values$x, objects))
  if(missing(xlim)) xlim = range(xvals)
  if(missing(xticks)) xticks = pretty(xlim)
  xvals = sort(c(xticks, xvals), decreasing=FALSE)
  # map labels to x-axis values
  xlabels = unlist(Map(xformat, xvals))

  if(missing(ylim1)) {
    ylim1 = range(unlist(Map(function(x) {
      if(x$yAxis == 1) {
        ans = range(c(0, x$values$y))
        if(!is.null(x$values$display)) ans = range(c(0, x$values$y, unlist(Map(function(z) sum(unlist(z)), x$values$display))))
        return(ans)
      }
      0
    }, objects)))
  }
  if(missing(ylim2)) {
    ylim2 = range(unlist(Map(function(x) {
      if(x$yAxis == 2) {
        ans = range(c(0, x$values$y))
        if(!is.null(x$values$display)) ans = range(c(0, x$values$y, unlist(Map(function(z) sum(unlist(z)), x$values$display))))
        return(ans)
      }
      0
    }, objects)))
  }
  options = sprintf("chart.xAxis.domain(%s);%s", toJSON(xlim), options)
  options = sprintf("%s;chart.yDomain1(%s);", options, toJSON(ylim1))
  options = sprintf("%s;chart.yDomain2(%s);", options, toJSON(ylim2))
  data = sprintf("var rdata=%s;\nvar xticks=%s;\nvar xvals=%s;\nvar xlabels=%s;\n",
             toJSON(objects, auto_unbox=TRUE), toJSON(xticks, auto_unbox=TRUE), toJSON(xvals), toJSON(xlabels))
  program = sprintf("
%s
nv.addGraph(function() {
  var chart = nv.models.multiChart()
                .useInteractiveGuideline(%s)
                .color(%s);
  chart.yAxis1.tickFormat(d3.format(',.1f'));
  chart.yAxis2.tickFormat(d3.format(',.1f'));
  chart.xAxis.tickValues(xticks)
             .ticks(xticks.length)
             .tickFormat(function (d) {ans = xlabels[xvals.indexOf(d)]; if(ans) {return ans}; return d;});
  chart.interpolate('%s');
  chart.tooltip.enabled(%s);
  %s
  d3.select(_this.svg)
      .datum(rdata)
      .call(chart);
  nv.utils.windowResize(chart.update);
  return chart;
});\n", data, guideline, toJSON(col), interpolate, tooltip, options)
  nvd3(program)
}
