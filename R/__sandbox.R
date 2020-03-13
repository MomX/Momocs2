x %>%
  dplyr::group_by(type) %>%
  # ni increment for each shape within each f level
  dplyr::mutate(ni=1:dplyr::n() - 1,
                # deduces the column index
                ci=1 + ni %% ncol) %>%
  dplyr::ungroup() %>%
  # deduces the row index
  dplyr::mutate(ri=cumsum(ifelse(ci==1, 1, 0))) -> df

nrow <- max(df$ri)
(nrow+1)-df$ri
