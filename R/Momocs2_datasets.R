#' Some bottles
#'
#' A dataset containing 40 bottles outlines
#'
#' @format A [mom_tbl] with:
#' \describe{
#'   \item{coo}{outline coordinates}
#'   \item{type}{of drink}
#'   \item{fake}{a fake vector}
#' }
#' @source todo
"bot"

#' Outlines of hearts
#'
#' A dataset containing 240 hand-drawn hearts with 4 landmarks
#'
#' @source We thank the fellows of the Ecology Department of
#' the French Institute of Pondicherry that drawn the hearts.
#' They then have been scanned, smoothed, scaled, centered,
#' and downsampled to 80 coordinates per outline.
"hearts"

#' @format A [coo_single] with:
#' \describe{
#'   \item{x}{coordinates}
#'   \item{y}{coordinates}
#' }
#' @source todo
"jb"

#' Outline of jb bottle
#'
#' A [coo_single] obtained with `pick(bot, "jb")`
#'
#' @format A [coo_single] with:
#' \describe{
#'   \item{x}{coordinates}
#'   \item{y}{coordinates}
#' }
#' @source todo
"jb"
