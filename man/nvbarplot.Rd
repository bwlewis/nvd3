% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nvbarplot.r
\name{nvbarplot}
\alias{nvbarplot}
\title{nv barplot}
\usage{
nvbarplot(
  height,
  space = 0.1,
  names.arg = NULL,
  beside = FALSE,
  horiz = FALSE,
  col = NULL,
  xlab = NULL,
  ylab = NULL,
  axes = TRUE,
  axisnames = TRUE,
  xaxt = "s",
  yaxt = "s",
  rotateLabels = 0,
  showControls = TRUE,
  tickNumFormat = ",.1f",
  ...
)
}
\arguments{
\item{height}{either a data.frame, vector or matrix of values describing the bars which
make up the plot.  If \code{height} is a vector, the plot consists
of a sequence of rectangular bars with heights given by the
values in the vector.  If \code{height} is a matrix and \code{beside}
is \code{FALSE} then each bar of the plot corresponds to a column
of \code{height}, with the values in the column giving the heights
of stacked sub-bars making up the bar.  If \code{height} is a
matrix and \code{beside} is \code{TRUE}, then the values in each column
are juxtaposed rather than stacked. If \code{height} is a data.frame
then an attempt to coerce it into matrix form is made using the
\code{dftab} function.}

\item{space}{bar group spacing (scalar value)}

\item{names.arg}{a vector of names to be plotted below each bar or group of
bars.  If this argument is omitted, then the names are taken
from the \code{names} attribute of \code{height} if this is a vector,
or the column names if it is a matrix.}

\item{beside}{a logical value.  If \code{FALSE}, the columns of \code{height} are
portrayed as stacked bars, and if \code{TRUE} the columns are
portrayed as juxtaposed bars.}

\item{horiz}{a logical value.  If \code{FALSE}, the bars are drawn vertically
with the first bar to the left.  If \code{TRUE}, the bars are
drawn horizontally with the first at the bottom.}

\item{col}{optional vector of bar component colors}

\item{xlab}{x-axis label}

\item{ylab}{y-axis label}

\item{axes}{if \code{TRUE} display the bar axis and its values.
\code{axes=FALSE} is equivalent to \code{yaxt='n'} when \code{horiz=FALSE}.}

\item{axisnames}{if \code{TRUE} display the group axis and its values.
\code{axisnames=FALSE} is equivalent to \code{xaxt='n'} when \code{horiz=FALSE}.}

\item{xaxt}{set to 'n' to suppress drawing x axis, otherwise plot the x axis.}

\item{yaxt}{set to 'n' to suppress drawing x axis, otherwise plot the x axis.}

\item{rotateLabels}{axis text rotation in degrees}

\item{showControls}{set to \code{TRUE} to display interactive controls}

\item{tickNumFormat}{format numeric tick labels using a d3.format string (d3.js)}

\item{...}{optional additional named plot options passed directly to nvd3.js (see examples)}
}
\value{
An htmlwidget object that is displayed using the object's show or print method.
(If you don't see your widget plot, try printing it with the \code{print} function.)
}
\description{
nv barplot
}
