# decent
.decent_decimals   <- function(x){}
.decent_linesize   <- function(x){}
.decent_pointsize  <- function(x){}
.decent_textsize   <- function(x){}



# manipulating attributes ---------------------------------
# given an object, add y to attribute "history"
# which creates it if does not exist yet
.add_history <- function(x, y=NULL){
  attributes(x)$history <- append(attributes(x)$history, y)
  x
}

# vectors and list helpers --------
# given an unnammed vector add names based on prefix+position_in_the_vector
.seq_naming_vector <- function(x, prefix=""){
  x %>% `names<-`(paste0(prefix, seq_along(x)))
}
# x <- 1:5
# names(x) # NULL
# .seq_naming_vector(x, "yo") %>% names()

# generalization for lists
.seq_naming_list <- function(x){
  purrr::imap(x, ~.seq_naming_vector(.x, .y))
}
# list(a=1:5, b=5:2) %>% .seq_naming_list()

.prefix_element_columns_with_list_names <- function(x){
  purrr::map2(x, names(x), ~.x %>% `colnames<-`(paste0(.y, "_", colnames(.x))))
}





