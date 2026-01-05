# dev.off()
# bot$coo %>%
#   p(xlim=c(-100, 500), ylim=c(-200, 1400))  %>%
#   outlines(col="grey20", lwd=0.2) %>%
#   # origin(cex=5) %>%
#   first_point() %>%
#   centroid()
#
#
# ## ggplot version
# bot2 <- bot$fac %>%
#   mutate(coo=map(bot$coo, ~.x %>% as_tibble() %>% `colnames<-`(c("x", "y"))),
#          name=names(bot))
# bot2 %>%
#   mutate(.i=1:n(), .before=everything()) %>%
#   unnest(coo) %>%
#   ggplot() + aes(x=x, y=y, group=.i) +
#   geom_path(linewidth = 0.25) +
#   # geom_point() +
#   coord_equal() +
#   labs(x = NULL, y = NULL) +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),
#         axis.ticks = element_line(colour = "gray50"),
#         axis.text = element_text(size = 8, colour = "gray50"),
#         panel.background = element_rect(linewidth = NA,  fill = NA)) -> gg
#
# gg + aes(col=type)
#
# gg + geom_point()
# gg + facet_grid(~type)
#
#
# gg_this <- function(x){
#   x %>%
#     mutate(.i=1:n(), .before=everything()) %>%
#     unnest(coo) %>%
#     ggplot() + aes(x=x, y=y, group=.i) +
#     geom_path(linewidth = 0.25) +
#     coord_equal() +
#     labs(x = NULL, y = NULL) +
#     theme_minimal()
# }
# # library(ggrastr)
# # Warning in install.packages :
# #   package ‘ggrastr’ is not available for this version of R
# #
# #
# # gg_this2 <- function(x){
# #   x %>%
# #     mutate(.i=1:n(), .before=everything()) %>%
# #     unnest(coo) %>%
# #     ggplot() + aes(x=x, y=y, group=.i) +
# #     geom_path(linewidth = 0.25) +
# #     coord_equal() +
# #     labs(x = NULL, y = NULL) +
# #     theme_minimal()
# # }
#
# add_first_points <- function(size = 2, colour = "red", ...) {
#   geom_point(
#     data = ~.x %>% group_by(.i) %>% slice(1),
#     size = size, colour = colour, ...
#   )
# }
#
# add_centroids <- function(size = 1, colour = "orange", shape="cross", ...) {
#   geom_point(
#     data = ~.x %>% group_by(.i) %>% summarise(x = mean(x), y = mean(y)),
#     size = size, colour = colour, shape=shape, ...
#   )
# }
#
# # Usage:
# gg + add_first_points() + add_centroids(shape = 3) + facet_grid(~type) + aes(col=type)
# gg + facet_wrap(~name) + facet_grid(~type)  +
#   theme_void() + scale_fill_viridis_d()
#
#
# gg + facet_wrap(~name) + aes(col=type)
# gg$data <- gg$data %>% arrange(type)
# gg +  aes(col=fct_inorder(type)) + facet_wrap(~name, )
#
#
