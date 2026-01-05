# # from Momocs:
# data(wings)
# wings %>% as_df %>% slice(1:20) -> wings
# wings$coo <- wings$coo %>% map(~{class(.x) <- c("ldk", class(.x)); .x})
# class(wings$coo) <- c("ldk", "coo", class(wings$coo))
# usethis::use_data(wings, overwrite=T)
#
# data(bot)
# bot %>% as_df %>% rename(dummy=fake) -> bot
# bot$coo <- bot$coo %>% map(~{class(.x) <- c("out", class(.x)); .x})
# class(bot$coo) <- c("out", "coo", class(bot$coo))
# usethis::use_data(bot, overwrite=T)
#
# shapes <- shapes %>% Momocs::slice(c(4, 7, 11, 15)) %>% coo_sample(120) %$% coo
# usethis::use_data(shapes, overwrite=T)
#
# data(Momocs:::hearts)
# hearts <- hearts %>% as_df() %>% mutate(ldk=hearts$ldk) %>%
#   select(coo, ldk, author=aut) %>% group_by(author) %>% slice(1:5) %>% ungroup()
# class(hearts$coo) <- c("out", "coo", class(hearts$coo))
# class(hearts$ldk) <- c("ldk_id", class(hearts$ldk))
# strange below...but toherise '$ doest work for atomic vectors'
# set.seed(2329)
# hearts$img <- replicate(40, paste0(c(sample(letters, 8), ".jpg"), collapse = ""))
# hearts <- dplyr::relocate(hearts, img) %>% dplyr::mutate(img=Momocs2::as_path(img))
# class(hearts$img) <- c("path", class(hearts$img))
# hearts <- hearts %>% select(img, coo, coo_ldk=ldk, author)
# usethis::use_data(hearts, overwrite=T)


#' Bottles dataset
#'
#' An outline toy dataset with the historical Momocs bottles dataset
#'
#' @format
#' A data frame with 20 rows and three columns
#' \describe{
#'   \item{coo}{outlines coordinates}
#'   \item{type}{of beverage}
#'   \item{dummy}{one more column to play with}
#' }
#' @source Images have been grabbed on the internet and
#' prepared by the package's authors. No particular choice has been
#' made on the dimension of the original images or the brands cited here.
#' An original idea that came up drinking chai in the courtyard of the
#' French Institute of Pondicherry circa 2011 an that eventually led to Momocs.
"bot"

#' Wings dataset
#'
#' An landmark toy dataset of mosquito of mosquito wings. Modified from Rohlf and Slice 1990.
#'
#' @format
#' A data frame with 20 rows and two columns
#' \describe{
#'   \item{coo}{18 landmark coordinates}
#'   \item{group}{species}
#' }
#' @source Rohlf and Slice 1990.
"wings"

#' Shapes dataset
#'
#' A list of 4 shapes (as outlines)
#'
#' @format a raw list with 4 shapes
#' @source Borrowed default shapes from (c) Adobe Photoshop. Do not send me to jail.
"shapes"

#' Heart dataset
#'
#' A list of 4 shapes (as outlines)
#'
#' @format
#' A data frame with 40 rows and three columns
#' \describe{
#'   \item{img}{dummy paths}
#'   \item{coo}{outline coordinates}
#'   \item{coo_ldk}{landmark id (4) on these coordinates}
#'   \item{author}{who drawn these}
#' }
#'
#' @source We thank the fellows of the Ecology Department of the
#' French Institute of Pondicherry circa 2011 that drawn the hearts.
#' They then have been landmarked, smoothed, scaled, centered, and
#' downsampled to 80 coordinates per outline.
"hearts"


