% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometry.R
\name{geometry}
\alias{geometry}
\alias{geometry_intersection_two_segments}
\alias{geometry_diff_two_vectors}
\alias{geometry_diff_two_segments}
\title{Geometry helpers ------}
\source{
Joseph O'Rourke :
\href{http://www.gamers.org/dEngine/rsc/usenet/comp.graphics.algorithms.faq}{compilation of graphics algorithm}
}
\usage{
geometry_intersection_two_segments(seg1, seg2)

geometry_diff_two_vectors(x1, y1, x2, y2)

geometry_diff_two_segments(seg1, seg2)
}
\arguments{
\item{seg1, seg2}{segment definitions as 2 rows (start, end) x 2 cols}

\item{x1, y1}{parameters (x and y diff) of vector 1}

\item{x2, y2}{parameters (x and y diff) of vector 2}
}
\description{
Geometry helpers ------
}
\section{Functions}{
\itemize{
\item \code{geometry_intersection_two_segments}: calculate intersection between two segments

\item \code{geometry_diff_two_vectors}: calculate scale and rotation differences between two vectors

\item \code{geometry_diff_two_segments}: calculate scale and rotation differences between two segments
}}

\seealso{
Other geometry: 
\code{\link{euclidean}}
}
\concept{geometry}
