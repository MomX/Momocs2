% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coo_modifyers.R
\name{coo_template}
\alias{coo_template}
\alias{coo_template.coo_list}
\alias{coo_template.mom_tbl}
\title{Templates shapes}
\usage{
coo_template(x, size = 1, from_col, to_col, ...)

\method{coo_template}{coo_list}(x, size = 1, ...)

\method{coo_template}{mom_tbl}(
  x,
  size = 1,
  from_col = coo,
  to_col = {     {         from_col     } },
  ...
)
}
\arguments{
\item{x}{\link{coo_single}, \link{coo_list} or \link{mom_tbl}}

\item{size}{\code{numeric} the side of the square inscribing the shape}

\item{from_col}{colnames from where to get the \link{coo_list}
and how to name the resulting one (only for \link{mom_tbl} method)}

\item{to_col}{colnames from where to get the \link{coo_list}
and how to name the resulting one (only for \link{mom_tbl} method)}

\item{...}{useless here}
}
\value{
a \link{coo_single}, \link{coo_list} or \link{mom_tbl}
}
\description{
Centers shape and scale them so that they are inscribed in a \code{size}-side square.
}
\section{Methods (by class)}{
\itemize{
\item \code{coo_list}: list method

\item \code{mom_tbl}: mom_tbl method
}}

\examples{
bot \%>\% pick(1) \%>\% coo_template() \%>\% gg()
bot \%>\% coo_template \%>\% pile()
}
\seealso{
Other coo_modifyers: 
\code{\link{coo_align}()},
\code{\link{coo_baseline}()},
\code{\link{coo_center}()},
\code{\link{coo_reflect}},
\code{\link{coo_rev}()},
\code{\link{coo_rotatecenter}()},
\code{\link{coo_rotate}()},
\code{\link{coo_sample_rr}()},
\code{\link{coo_sample}()},
\code{\link{coo_scale}()},
\code{\link{coo_shear}()},
\code{\link{coo_slide}()},
\code{\link{coo_split}()},
\code{\link{coo_trans}()},
\code{\link{coo_trim}()},
\code{\link{coo_up}()}

Other scalings: 
\code{\link{coo_scale}()}
}
\concept{coo_modifyers}
\concept{scalings}
