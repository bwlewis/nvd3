#' nv barplot
#' 
#' @param height either a data.frame, vector or matrix of values describing the bars which
#'          make up the plot.  If \code{height} is a vector, the plot consists
#'          of a sequence of rectangular bars with heights given by the
#'          values in the vector.  If \code{height} is a matrix and \code{beside}
#'          is \code{FALSE} then each bar of the plot corresponds to a column
#'          of \code{height}, with the values in the column giving the heights
#'          of stacked sub-bars making up the bar.  If \code{height} is a
#'          matrix and \code{beside} is \code{TRUE}, then the values in each column
#'          are juxtaposed rather than stacked. If \code{height} is a data.frame
#'          then an attempt to coerce it into matrix form is made using the
#'          \code{dftab} function.
#' @param space bar group spacing (scalar value)
#' @param names.arg a vector of names to be plotted below each bar or group of
#'          bars.  If this argument is omitted, then the names are taken
#'          from the \code{names} attribute of \code{height} if this is a vector,
#'          or the column names if it is a matrix.
#' @param beside a logical value.  If \code{FALSE}, the columns of \code{height} are
#'          portrayed as stacked bars, and if \code{TRUE} the columns are
#'          portrayed as juxtaposed bars.
#' @param horiz a logical value.  If \code{FALSE}, the bars are drawn vertically
#'          with the first bar to the left.  If \code{TRUE}, the bars are
#'          drawn horizontally with the first at the bottom.
#' @param col optional vector of bar component colors
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param xaxt set to 'n' to suppress drawing x axis, otherwise plot the x axis.
#' @param yaxt set to 'n' to suppress drawing x axis, otherwise plot the x axis.
#' @param axes if \code{TRUE} display the bar axis and its values.
#'        \code{axes=FALSE} is equivalent to \code{yaxt='n'} when \code{horiz=FALSE}.
#' @param axisnames if \code{TRUE} display the group axis and its values.
#'        \code{axisnames=FALSE} is equivalent to \code{xaxt='n'} when \code{horiz=FALSE}.
#' @param rotateLabels axis text rotation in degrees
#' @param showControls set to \code{TRUE} to display interactive controls
#' @param tickNumFormat format numeric tick labels using a d3.format string (d3.js)
#' @param ... optional additional named plot options passed directly to nvd3.js (see examples)
#' @return
#' An htmlwidget object that is displayed using the object's show or print method.
#' (If you don't see your widget plot, try printing it with the \code{print} function.)
#' @importFrom jsonlite toJSON
#' @export
nvbarplot = function(height, space = 0.1, names.arg = NULL, beside = FALSE,
             horiz = FALSE, col = NULL, xlab = NULL, ylab = NULL,
             axes = TRUE, axisnames = TRUE, xaxt = 's', yaxt = 's',
             rotateLabels=0, showControls=TRUE, tickNumFormat=",.1f", ...)
{
  if(is.data.frame(height)) height = dftab(height)
  if(!is.matrix(height)) {
    height = rbind(height)
    rownames(height) = NULL
    showControls = FALSE
  }
  if(NROW(height) == 1) showControls = FALSE
  height = t(height) # XXX fix this sillyness
  if(!is.null(names.arg)) {
    if(length(names.arg) < nrow(height)) names.arg = c(names.arg, rep("", length.out=nrow(height) - length(names.arg)))
    rownames(height) = names.arg
  }
  showXAxis = ifelse(xaxt == 'n' || (!axisnames), FALSE, TRUE)
  showYAxis = ifelse(yaxt == 'n' || (!axes), FALSE, TRUE)
  if(is.null(rownames(height))) rownames(height) = seq(nrow(height))
  data = sprintf("var rdata=%s;\nvar xlabels=%s;\n", multibar_matrix2json(height), toJSON(rownames(height)))
  barColor = ""
  if(!is.null(col)) barColor = sprintf(".barColor(%s)\n", toJSON(col))
  options = list(...)
  if(length(options) > 0)
  {
    no = names(options)
    names(options) = NULL
    options = paste(lapply(seq_along(no),
                    function(i) sprintf("chart.%s(%s);", no[i], jsonlite::toJSON(options[[i]], auto_unbox=TRUE))), collapse="\n")
  } else options = ""
  chart = sprintf("
var chart=nv.models.multiBarChart()%s
            .rotateLabels(%s)
            .showControls(%s)
            .groupSpacing(%s)
            .showXAxis(%s)
            .showYAxis(%s)
            .stacked(%s);\n",
             barColor,
             toJSON(rotateLabels, auto_unbox=TRUE),
             toJSON(showControls, auto_unbox=TRUE),
             toJSON(space, auto_unbox=TRUE),
             toJSON(showXAxis, auto_unbox=TRUE),
             toJSON(showYAxis, auto_unbox=TRUE),
             toJSON(!beside, auto_unbox=TRUE))
  program = sprintf("
%s
nv.addGraph(function() {
  %s%s
  chart.xAxis.tickFormat(function (d) { return xlabels[d];});
  chart.yAxis.tickFormat(d3.format('%s'));
  d3.select(_this.svg)
      .datum(rdata)
      .call(chart);
  nv.utils.windowResize(chart.update);
  return chart;
});", data, chart, options, tickNumFormat)

  nvd3(program)
}

#' @rdname nvd3-shiny
#' @export
nvOutput <- function(outputId, width="100%", height="600px") {
    shinyWidgetOutput(outputId, "nv", width, height, package = "nvd3")
}

#' rendernvbarplot
#' @export
rendernvbarplot <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) expr <- substitute(expr) # force quoted
    shinyRenderWidget(expr, nvOutput, env, quoted = TRUE)
}
