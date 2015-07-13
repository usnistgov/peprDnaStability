## figures for exploring annotated gels and lanes intensities

annotated_gel <- function(gel_mat, lane_edge_df, peaks_df, lane_labels){
    gel_df <- as.data.frame(gel_mat) %>%
                dplyr::mutate(y = 1:n()) %>%
                tidyr::gather("x","intensity",-y) %>%
                dplyr::mutate(x = as.numeric(stringr::str_sub(x, 2,-1L)))

    lane_centers <- with(lane_edge_df, (x[-1] + x[-length(x)])/2)

    print(ggplot2::ggplot(gel_df) +
            ggplot2::geom_raster(ggplot2::aes(x ,-y, fill = intensity)) +
            ggplot2::scale_fill_gradient(low = "white", high = "black") +
            ggplot2::geom_vline(data = lane_edge_df,
                                ggplot2::aes(xintercept = x),
                                color = "blue", linetype = 2, alpha = 0.75) +
            ggplot2::geom_hline(data = peaks_df, ggplot2::aes(yintercept = -x),
                                color = "orange", linetype = 2, alpha = 0.75) +
            ggplot2::scale_x_continuous(breaks = lane_centers,
                                        labels = lane_labels,
                                        expand = c(0,0)) +
            ggplot2::scale_y_continuous(breaks = -peaks_df$x,
                                        labels = peaks_df$ladder,
                                        expand = c(0,0)) +
            ggplot2::theme_bw() +
            ggplot2::labs(y = "DNA Fragment Size (kb)",
                          x = "Sample") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                           axis.ticks.x = ggplot2::element_blank(),
                           legend.position = "none",
                           panel.border = ggplot2::element_rect(size = 0)))
}

annotated_gel_clean <- function(gel_mat, lane_edge_df, peaks_df, lane_labels){
    gel_df <- as.data.frame(gel_mat) %>%
        dplyr::mutate(y = 1:n()) %>%
        tidyr::gather("x","intensity",-y) %>%
        dplyr::mutate(x = as.numeric(stringr::str_sub(x, 2,-1L)))

    lane_centers <- with(lane_edge_df, (x[-1] + x[-length(x)])/2)
    print(ggplot2::ggplot(gel_df) +
              ggplot2::geom_raster(ggplot2::aes(x ,-y, fill = intensity)) +
              ggplot2::scale_fill_gradient(low = "white", high = "black") +
#               ggplot2::geom_vline(data = lane_edge_df,
#                                   ggplot2::aes(xintercept = x),
#                                   color = "blue", linetype = 2, alpha = 0.75) +
#               ggplot2::geom_hline(data = peaks_df, ggplot2::aes(yintercept = -x),
#                                   color = "orange", linetype = 2, alpha = 0.75) +
              ggplot2::scale_x_continuous(breaks = lane_centers,
                                          labels = lane_labels,
                                          expand = c(0,0)) +
              ggplot2::scale_y_continuous(breaks = -peaks_df$x,
                                          labels = lane_labels,
                                          expand = c(0,0)) +
              ggplot2::theme_bw() +
              ggplot2::labs(y = "DNA Fragment Size (kb)",
                            x = "Sample") +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                             axis.ticks.x = ggplot2::element_blank(),
                             legend.position = "none",
                             panel.border = ggplot2::element_rect(size = 0)))
}

lane_intensity_profile_figure <- function(intensity_df, peaks_df, gel_meta){
    intensity_df <- intensity_df %>%
        tidyr::gather("lane", "intensity", -x) %>%
        dplyr::right_join(gel_meta)

    print(ggplot2::ggplot(intensity_df) +
            ggplot2::geom_path(ggplot2::aes(x, intensity, group = lane)) +
            ggplot2::geom_vline(data= peaks_df, ggplot2::aes(xintercept = x),
                                linetype = 2, color = "orange") +
            ggplot2::facet_wrap(~condition, ncol = 1) +
            ggplot2::theme_bw() +
            ggplot2::labs(y = "Intensity",
                          x = "Fragment Size in kb") +
            ggplot2::scale_x_continuous(breaks = peaks_df$x,
                                        labels = peaks_df$ladder) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)))
}
