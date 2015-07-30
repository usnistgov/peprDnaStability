##
## Methods for finding local max for gel peak heights
##

# normalize intensity values by total area under the curve for each sample
norm_intensity <- function(intensity_df){
    long_df <- intensity_df %>% tidyr::gather("lane","intensity", 2:ncol(intensity_df)) %>%
        dplyr::group_by(lane) %>%
        dplyr::mutate(norm_intensity = intensity/sum(intensity))
}

# Identify ladder peaks
findpeaks <- function(vec,bw=1,x.coo=c(1:length(vec))){
    # function code from http://www.r-bloggers.com/an-algorithm-to-find-local-extrema-in-a-vector/
    pos.x.max <- NULL; pos.y.max <- NULL; pos.x.min <- NULL; pos.y.min <- NULL
    for(i in 1:(length(vec)-1)){
        if((i+1+bw)>length(vec)){
            sup.stop <- length(vec)}else{sup.stop <- i+1+bw
            }
        if((i-bw)<1){inf.stop <- 1}else{inf.stop <- i-bw}
        subset.sup <- vec[(i+1):sup.stop]
        subset.inf <- vec[inf.stop:(i-1)]

        is.max   <- sum(subset.inf > vec[i]) == 0
        is.nomin <- sum(subset.sup > vec[i]) == 0

        no.max   <- sum(subset.inf > vec[i]) == length(subset.inf)
        no.nomin <- sum(subset.sup > vec[i]) == length(subset.sup)

        if(is.max & is.nomin){
            pos.x.max <- c(pos.x.max,x.coo[i])
            pos.y.max <- c(pos.y.max,vec[i])
        }
        if(no.max & no.nomin){
            pos.x.min <- c(pos.x.min,x.coo[i])
            pos.y.min <- c(pos.y.min,vec[i])
        }
    }
    return(list(pos.x.max,pos.y.max,pos.x.min,pos.y.min))
}

# get marker positions
calc_ladder_markers <- function(norm_df, ladder_lanes, r1_list, r2_list, r3_list){
    # filtering by even positions to prevent multiple peaks for the same marker
    peak_df <- dplyr::data_frame()
    for(j in list(r1_list, r2_list, r3_list)){
        leftset <- as.numeric(j['leftset'])
        rightset <- as.numeric(j['rightset'])
        bw <- as.numeric(j['bw'])
        n_peaks <- as.numeric(j['npeaks'])
        x <- c(); y <- c(); lane <- c()
        if(!ladder_lanes[1] %in% norm_df$lane){
            ladder_lane_labels <- paste0("L", ladder_lanes)
        }else{
            ladder_lane_labels <- ladder_lanes
        }

        for(i in ladder_lane_labels){
            df_x <- unique(norm_df$x)[c(leftset:rightset)]
            even_x <- df_x[which(df_x %% 2 == 0)]
            marker_df <- norm_df  %>%
                dplyr::filter(lane == i, x %in% even_x)
            peaks <- marker_df$norm_intensity %>% findpeaks(bw)
            x <- c(x, peaks[[1]]*2+leftset); y <- c(y, peaks[[2]])
            lane <- c(lane, rep(i, length(peaks[[1]])))
        }
        peak_df <- dplyr::data_frame(x, y, lane) %>%
            dplyr::group_by(lane,y) %>%
            dplyr::summarise(x = max(x)) %>%
            dplyr::group_by(lane) %>%
            dplyr::top_n(n = n_peaks, wt = y) %>%
            dplyr::bind_rows(peak_df)
    }
    peak_df %>% dplyr::arrange(lane, x)
}

# check marker placement
check_marker_plot <- function(norm_df, peak_df, ladder_lanes){
    print(norm_df)
    marker_lanes <- norm_df %>% dplyr::filter(lane %in% ladder_lanes)#paste0("L", ladder_lanes))
    print(marker_lanes)
    ggplot2::ggplot() +
        ggplot2::geom_line( data = marker_lanes,
                            ggplot2::aes(x = x, y = norm_intensity),
                            color = "darkblue") +
        ggplot2::geom_point(data = peak_df,
                            ggplot2::aes(x,y), color = "orange") +
        ggplot2::geom_vline(data = peak_df,
                            ggplot2::aes(xintercept = x),
                            color = "orange", linetype = 2) +
        ggplot2::theme_bw() +
        ggplot2::facet_wrap(~lane) +
        ggplot2::labs(x = "Vertical Pixel Position", y = "Normalized Intensity")
}

add_marker <- function(peak_df, marker_id){
    peak_df %>% dplyr::ungroup() %>%
        dplyr::mutate(ladder = rep(marker_id,times = 2))
}
