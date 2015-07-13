#' Pipeline for processing gel images
#'
#' @param gel_dat Rdata output from gel processing app
#' @param gel_file gel image file
#'
#' @return list
#' @export
#'
#' @examples
process_gel <- function(gel_dat){
    load(gel_dat)
    image_file <- paste0(dirname(gel_dat),"/", filename)
    gel_mat <- readGel(image_file)
    lane_edge_list <- lane_edges(gel_mat, lanes)
    intensity_df <- lane_intensities(lane_edge_list[['lane_edges_df']], gel_mat, lanes)
    norm_df <- norm_intensity(intensity_df)
    peak_df <- calc_ladder_markers(norm_df, ladder_lanes, r1_list, r2_list, r3_list) %>%
        add_marker(marker_labels)
    bin_df <- add_marker_bins(norm_df, peak_df, marker_labels) %>%
        calc_bin_props()
    return(list(gel = filename,
                intensity_dat = norm_df,
                marker_dat = peak_df,
                image_dat = gel_mat,
                lane_edge_list,
                bin_dat = bin_df))
}


#' Batch processing gel
#'
#' @param data_dir directory with image and Rdata files
#'
#' @return list processed data for each image
#' @export
#'
#' @examples
batch_process_gels <- function(data_dir){
    Rdat_files <- list.files(path =data_dir,
                             pattern = "Rdata",
                             full.names = TRUE)
    batch_dat <- lapply(X = Rdat_files, FUN = process_gel)
    names(batch_dat) <- lapply(Rdat_files, FUN = basename)  %>%
        unlist()  %>%
        gsub(".Rdata","", .)
    return(batch_dat)
}
