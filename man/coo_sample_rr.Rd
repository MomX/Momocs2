% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coo_modifyers.R
\name{coo_sample_rr}
\alias{coo_sample_rr}
\title{Sample shapes using the regular radius method}
\usage{
coo_sample_rr(x, n, from_col, to_col, ...)
}
\arguments{
\item{x}{\link{coo_single}, \link{coo_list} or \link{mom_tbl}}

\item{n}{\code{integer} desired number of coordinates (required)}

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
Sample coordinates with regular angles
}
\examples{
bot \%>\% pick(1)  # todo
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
\code{\link{coo_sample}()},
\code{\link{coo_scale}()},
\code{\link{coo_shear}()},
\code{\link{coo_slide}()},
\code{\link{coo_split}()},
\code{\link{coo_template}()},
\code{\link{coo_trans}()},
\code{\link{coo_trim}()},
\code{\link{coo_up}()}
}
\concept{coo_modifyers}
