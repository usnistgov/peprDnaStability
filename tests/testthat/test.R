# reading example gel file
library(dplyr)
library(ggplot2)
library(peprrDnaStability)
gel_mat <- readGel("inst//extdata//example_gel.jpg")
lane_edge_list <- lane_edges(gel_mat)
check_lane_plot(gel_cols = lane_edge_list[['col_intensity']],
                lane_edges_df = lane_edge_list[['lane_edges_df']])
intensity_df <- lane_intensities(lane_edge_list[['lane_edges_df']], gel_mat)
norm_df <- norm_intensity(intensity_df)

## values for example image
r1_list = list(bw=5, npeaks = 4, leftset = 10, rightset = 547)
r2_list = list(bw=5, npeaks = 3, leftset = 547, rightset = 1150)
r3_list = list(bw=5, npeaks = 2, leftset = 1150, rightset = 2000)

peak_df <- calc_ladder_markers(norm_df, r1_list, r2_list, r3_list)

check_marker_plot(norm_df, peak_df)

# annotated gel
gel_meta <- read.csv("inst//extdata//example_metadata.csv",
                     stringsAsFactors = F) %>%
                    mutate(lane = paste0("L",lane))
lane_labels <- gel_meta %>% .$condition %>% as.character()

annotated_gel(gel_mat, lane_edge_list[['lane_edges_df']], peak_df,lane_labels)

peak_marker <- add_marker(peak_df, 1:9) %>% group_by(ladder) %>% summarize(x = mean(x))
lane_intensity_profile_figure(intensity_df = intensity_df,
                              peaks_df = peak_marker,
                              gel_meta = gel_meta)

annotated_gel_clean(gel_mat, lane_edge_list[['lane_edges_df']], peak_df,lane_labels)

## binning data
binned_dat <- add_marker_bins(norm_df, peak_df, 1:9)

## calculating proportions
bin_props <- cal_bin_props(binned_dat)
