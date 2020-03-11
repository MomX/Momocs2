# manipulating attributes ---------------------------------
# given an object, add y to attribute "history"
# which creates it if does not exist yet
.add_history <- function(x, y=NULL){
  attributes(x)$history <- append(attributes(x)$history, y)
  x
}

# messaging -----------------------------------------------
.msg_info    <- cli::cli_alert_info
.msg_danger  <- cli::cli_alert_danger
.msg_warning <- cli::cli_alert_warning
.msg_success <- cli::cli_alert_success

# checking ------------------------------------------------
.check <- function(cond_to_pass, mess_if_not=""){
  if (!cond_to_pass)
      rlang::abort(mess_if_not)
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

