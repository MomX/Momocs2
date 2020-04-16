# messaging -----------------------------------------------
.msg_info    <- cli::cli_alert_info
.msg_danger  <- cli::cli_alert_danger
.msg_warning <- cli::cli_alert_warning
.msg_success <- cli::cli_alert_success

# checking ------------------------------------------------
.check <- function(cond_to_pass, mess_if_not=""){
  if (!cond_to_pass)
    .msg_danger(mess_if_not)
}

# todo proper stop (?abort)
# here

# standardizes message when no method
not_defined <- function(fun=""){
  paste0(fun, ": no method defined on this class") %>%
    .msg_info()
}

# negate missing
provided <- function(x) !missing(x)

# hep with this boring task
deprecated <- function(old, new){
  paste0(old, " is deprecated. Please use ",
         new, " (and see ?",  new, ")") %>%
    .msg_warning()
}
#.deprecated("plop", "plip")


