% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.r, R/shiny.r
\name{nvd3-shiny}
\alias{nvd3-shiny}
\alias{nvOutput}
\title{Shiny bindings for nvd3 widgets}
\usage{
nvOutput(outputId, width = "100\%", height = "600px")
}
\arguments{
\item{outputId}{output variable to read from}

\item{width, height}{Must be a valid CSS unit (like \code{"100\%"},
\code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
string and have \code{"px"} appended.}

\item{expr}{An expression that generates nvd3 graphics.}

\item{env}{The environment in which to evaluate \code{expr}.}

\item{quoted}{Is \code{expr} a quoted expression (with \code{quote()})? This
is useful if you want to save an expression in a variable.}
}
\description{
Output and render functions for using nvd3 widgets within Shiny
applications and interactive Rmd documents.
}
