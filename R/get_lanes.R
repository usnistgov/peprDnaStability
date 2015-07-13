# ## functions for finding lane edges from gel matrix
# update to take uncroped image as input
lane_edges <- function(gel_mat){
    gel_cols <- gel_mat  %>% .get_gel_cols()
    lane_edges_df <- .find_lane_edges(gel_cols, return_df = TRUE)
    list(col_intensity = gel_cols,
         lane_edges_df = lane_edges_df)
}

.get_gel_cols <- function(gel_img){
    colSums(gel_img)
}

## splits gel into number of equal size regions and returns minimum of each
## region as a vector
.find_lane_edges <- function(gel_cols, lanes = 12, return_df = FALSE){
    # finding lane edges
    nedges <- lanes + 1
    bins <- cut(1:length(gel_cols),c(0:nedges * length(gel_cols)/nedges))
    col_df <- dplyr::data_frame(x = 1:length(gel_cols), y = gel_cols,bins)

    col_min <- col_df %>%
                    dplyr::group_by(bins) %>%
                    dplyr::summarize(y = min(y))
    lane_edges_df <- dplyr::inner_join(col_min, col_df) %>%
        dplyr::group_by(bins,y) %>%  dplyr::summarize(x = mean(x))
    if(return_df){
       return(lane_edges_df)
    }else{
        lane_edges_df %>%
            .$x %>%
            return()
    }
}

## generate data frame with lane profile intensities
lane_intensities <- function(lane_edge_df, gel_mat){
    gel_df <- t(gel_mat) %>% as.data.frame() %>%
                dplyr::tbl_df() %>%
                dplyr::mutate(x = 1:n(),
                       lanes = cut(x, breaks = lane_edge_df$x,
                                   labels = paste0("L",1:12))) %>%
                dplyr::filter(!is.na(lanes)) %>%
                dplyr::group_by(lanes) %>%
                dplyr::summarise_each(dplyr::funs(sum))
    gel_df$x <- NULL
    gel_df %>%
        tidyr::gather("y","intensity", 2:ncol(gel_df)) %>%
        tidyr::spread(lanes, intensity) %>%
        dplyr::mutate(y = 1:n()) %>%
        dplyr::rename(x = y)
}

check_lane_plot <- function(gel_cols, lane_edges_df){
    # checking lane breaks
    col_df <- dplyr::data_frame(x = 1:length(gel_cols), y = gel_cols)
    print({ggplot2::ggplot() +
              ggplot2::geom_line(data = col_df,
                                 ggplot2::aes(x,y),
                                 color = "darkblue") +
              ggplot2::geom_vline(data = lane_edges_df,
                                  ggplot2::aes(xintercept = x),
                                  color = "orange", linetype = 2) +
              ggplot2::geom_point(data = lane_edges_df,
                                  ggplot2::aes(x, y),
                                  color = "orange") +
              ggplot2::theme_bw() +
              ggplot2::labs(title = "Predicted Lane Edges",
                            x = "X-Axis Pixel Column",
                            y = "Column Total Intensity")
    })
}
