% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coo_modifyers.R
\name{coo_trans}
\alias{coo_trans}
\title{Translate shapes}
\usage{
coo_trans(x, x_trans = 0, y_trans = 0, from_col, to_col, ...)
}
\arguments{
\item{x}{\link{coo_single}, \link{coo_list} or \link{mom_tbl}}

\item{x_trans, y_trans}{\code{numeric} how much translate on each direction}

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
Returns a shape translated by \code{x_trans} and \code{y_trans}.
}
\examples{

bot \%>\% pick() \%>\% coo_center() \%>\% coo_trans(5, 5) \%>\% gg()

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
\code{\link{coo_template}()},
\code{\link{coo_trim}()},
\code{\link{coo_up}()}

Other translations: 
\code{\link{coo_center}()}
}
\concept{coo_modifyers}
\concept{translations}
