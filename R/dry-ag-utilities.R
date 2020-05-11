read_geo_tiff <- function(file_name, tiff_dir, names = NULL,
                          is_stack = TRUE, verbose = TRUE) {
    file_path <- file.path(tiff_dir, file_name)
    if(!file.exists(file_path))
        stop(sprintf("The file '%s' does not exist", file_path))
    else {
        ret <- if(is_stack == TRUE)
                   stack(file_path)
               else
                   raster(file_path)
        if(!is.null(names))
            names(ret) <- names}

    if(verbose == TRUE)
        message(sprintf("Reading '%s'", file_path))

    ret
}
