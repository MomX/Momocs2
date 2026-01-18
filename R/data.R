# # from Momocs:
# data(wings)
# wings %>% as_df %>% slice(1:20) -> wings
# wings$coo <- wings$coo %>% map(~{class(.x) <- c("ldk", class(.x)); .x})
# class(wings$coo) <- c("ldk", "coo", class(wings$coo))
# usethis::use_data(wings, overwrite=T)
#
# data(bot)
# bot <- Momocs::bot %>% Momocs::as_df() %>% dplyr::mutate(id=names(coo), .before=1)
# bot$coo <- bot$coo %>% purrr::map(as_xy)
# class(bot$coo) <- c("out", "coo", class(bot$coo))
# set.seed(2329)
# bot <- bot %>% dplyr::mutate(price=runif(dplyr::n(), 1, 4) %>% round(1))
# usethis::use_data(bot, overwrite=T)

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
# hearts <- hearts %>% select(img, coo, coo_ldk=ldk, author) %>% mutate(path=Momocs2::as_path(img))
# usethis::use_data(hearts, overwrite=T)


#' Bottles dataset
#'
#' An outline toy dataset with the historical Momocs bottles dataset
#'
#' @format
#' A data frame with 20 rows and three columns
#' \describe{
#'   \item{id}{unique id, here brand name}
#'   \item{coo}{outlines coordinates}
#'   \item{type}{of beverage}
#'   \item{fake}{one more column to play with}
#'   \item{price}{one completely random numeric column}
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

# z <- Momocs::olea %>% Momocs::as_df()
# z %>% dplyr::mutate(id=paste0(var, "_", ind)) %>%
#   tidyr::pivot_wider(names_from = view, values_from=coo) %>%
#   na.omit() -> z
# class(z$VL) <- class(z$VD) <- c("cur", "coo", "list")
# z %>% front() %>%
#   dplyr::group_by(var) %>%
#   dplyr::mutate(ind=1:dplyr::n(),
#                 id=paste0(stringr::str_sub(var, 1, 2), 1:dplyr::n())) %>%
#   dplyr::select(id, VD, VL, var, status=domes) %>%
#   dplyr::ungroup() -> olea
# olea <- olea[!(purrr::map_lgl(olea$VL, ~is.null(.x)) | purrr::map_lgl(olea$VL, ~is.null(.x))), ]
# olea$var <- factor(olea$var)
# usethis::use_data(olea, overwrite=T)

#' Olea dataset
#'
#' An open curves dataset with two views derived from the legacy Momocs olea dataset.
#'
#' Olive stones, photographed using two orthogonal views.
#'
#' @format
#' A data frame with 90 rows (three accessions):
#' \describe{
#'   \item{id}{unique id}
#'   \item{VD}{curves coordinates for dorsal view}
#'   \item{VL}{curves coordinates for lateral view}
#'   \item{var}{olive accession name}
#'   \item{status}{factor, whether accession is wild or cultivated}
#' }
#' @source We thank Jean-Frederic Terral and Sarah Ivorra
#' (UMR CBAE, Montpellier, France) from allowing us to share the data.
#'
#' You can have a look to the original paper:
#' Terral J-F, Alonso N, Capdevila RB i, Chatti N, Fabre L, Fiorentino G,
#' Marinval P, Jorda GP, Pradat B, Rovira N, et al. 2004.
#' Historical biogeography of olive domestication (_Olea europaea_ L.)
#' as revealed by geometrical morphometry applied to biological and archaeological material.
#' _Journal of Biogeography_ 31: 63-77.
"olea"

