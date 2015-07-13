ttest_to_row <- function(x,y,bin, gel_rep, treatment){
    tt <- t.test(x,y)
    data_frame(size_bins = bin,condition = treatment, gel= gel_rep,
               pvalue = tt$p.value, sig = tt$p.value < 0.1)
}

make_ttest_df <- function (dat, value) {
    tt_df <- data_frame()
    ## Pairwise comparison to control for each bin and each condition
    for(gel_rep in unique(dat$gel)){
        for(bin in unique(dat$bins)){
            df <- dat %>% filter(bins == bin & gel == gel_rep)
            y <- df  %>% filter(condition == "Control")  %>% .[[value]]
            for(treatment in unique(df$condition[df$condition != "Control"])){
                x <-df  %>% filter(condition == treatment)  %>% .[[value]]
                tt_df <- bind_rows(tt_df, ttest_to_row(x,y,bin,gel_rep, treatment))
            }
        }
    }
    tt_df
}
