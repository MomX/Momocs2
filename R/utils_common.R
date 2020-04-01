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
