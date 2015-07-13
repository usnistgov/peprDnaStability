add_marker_bins <- function(norm_df, peak_df, marker_id){
    peak_df <- peak_df %>% dplyr::ungroup() %>%
        dplyr::mutate(ladder = rep(marker_id,times = 2)) %>%
        dplyr::group_by(ladder) %>%
        dplyr::summarize(x = mean(x))
    ladder_labels <- paste(c(0,marker_id), c(marker_id, 999), sep = "-")
    norm_df %>% dplyr::mutate(bins = cut(x,
                                         breaks = c(0, peak_df$x,
                                                    nrow(norm_df)),
                                         labels = ladder_labels))
}

calc_bin_props <- function(binned_df){
    binned_df %>%
        dplyr::group_by(lane, bins) %>%
        dplyr::summarize(bin_intensity = sum(norm_intensity))
}


