## Loading gel

## retrieving first level of gel image matrix
gel_to_mat <- function(gel_img){
    #check values for each pixel - all the same
    if(length(dim(gel_img)) == 2){
        return(gel_img)
    }
    if(sum(gel_img[,, 1] != gel_img[,, 2]) ||
           sum(gel_img[,, 2] != gel_img[,, 3])){
        warning("Matrix levels are not equal, convert image to grey scale")
    }
    # working with first matrix
    gel_img[,,1]
}

#' function for loading PFGE gel images
#'
#' @param path path to gel image
#'
#' @return image matrix
#' @export
#'
#' @examples readGel("/path/to/gel.jpeg")
readGel <- function(path){
    if(tools::file_ext(path) %in% c("jpeg","jpg")){
        jpeg::readJPEG(path) %>%
            gel_to_mat() %>%
            return()
    }else{
        stop("jpeg input file required, with extension 'jpeg' or 'jpg'")
    }
}

## TO DO
## add functions for other image types



