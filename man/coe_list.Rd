% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mom_coo_coe_classes.R
\name{coe_list}
\alias{coe_list}
\alias{new_coe_list}
\alias{vec_ptype_full.coe_list}
\alias{vec_ptype_abbr.coe_list}
\alias{is_coe_list}
\alias{is_coe_list1}
\title{Create list of coe_single}
\usage{
new_coe_list(x = list())

coe_list(x)

vec_ptype_full.coe_list(x)

vec_ptype_abbr.coe_list(x)

is_coe_list(x)

is_coe_list1(x)
}
\arguments{
\item{x}{a list of \link{coe_single}}
}
\value{
a \code{coe_list} object
}
\description{
\code{coe_list} objects are \link{list} of \link{coe_single}.
}
\details{
They behave like lists
and thus as regular \link{tibble}/\link{data.frame} columns.
The only difference is that and are understood by morphometric methods used in MomX.
}
\section{Functions}{
\itemize{
\item \code{new_coe_list}: Constructor

\item \code{coe_list}: Helper

\item \code{is_coe_list}: Class tester

\item \code{is_coe_list1}: Class1 tester
}}

\examples{
matrix(1:12, nrow=1) \%>\% coe_single() \%>\% coo_list()
}
