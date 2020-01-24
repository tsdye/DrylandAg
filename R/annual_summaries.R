check_default_palette <- function(test_palette) {
    default_palettes <- c("viridis", "plasma", "inferno")
    if(is.element(test_palette, default_palettes))
        return(test_palette)
    else
        stop(sprintf("Default palette '%s' is not recognized", default_palette))
}

check_palette <- function(test_palette) {
    palettes <- c("bright", "contrast", "vibrant", "muted")
    if(is.element(test_palette, palettes))
        return(test_palette)
    else
        stop(sprintf("Palette '%s' is not recognized", default_palette))
}

get_data <- function(data, input_file_name) {
    if(is.null(data) & is.null(input_file_name))
        stop("One of 'data' and 'input_file_name' required")
    if(!is.null(data) & !is.null(input_file_name))
        stop("Only one of 'data' and 'input_file_name' required")
    if(!is.null(input_file_name))
        if(!file.exists(input_file_name))
            stop(sprintf("The input file %s does not exist", input_file_name))
        else
            ret <- readRDS(file = input_file_name)
    else
        ret <- data
    ret
}

choose_palette <- function(data, palette, default_palette) {
    max_colors <- c(bright = 7, contrast = 3, vibrant = 7, muted = 9)

    if(length(data$rasters) > max_colors[palette]) {
        message(sprintf("Requested %i colors but '%s' palette offers %i. Using default palette, '%s'.",
                        length(data$rasters), palette, max_colors[palette], default_palette))
        return(TRUE)
    }
    else {
        message(sprintf("Preparing color output using the %s palette", palette))
        return(FALSE)
    }
}

## Utility function to generate file paths that can be used in series
filename_splice <- function(pathname, filename, splice) {
    name_parts <- strsplit(basename(filename), ".", fixed = TRUE)
    file.path(pathname, paste0(name_parts[[1]][1], splice, ".",
                               name_parts[[1]][2]))
}

## function for plotting comparative graphs
comparative_graph <- function(data,
                              out_file,
                              palette,
                              default_palette,
                              use_default_palette,
                              height = 7, width = 7) {
    if(use_default_palette == TRUE) {
        p <- ggplot(data = data, aes(x = polygon_name, y = value, fill = polygon_name))
        p <- p + geom_boxplot(outlier.shape = 20)
        p <- p + scale_fill_viridis_d(option = default_palette)
        ## p <- p + guides(fill = guide_legend(nrow = 5))
        p <- p + labs(y = "Value", x = NULL, fill = "Place")}
    else {
            p <- ggplot(data = data, aes(x = value,
                                         fill = polygon_name,
                                         color = polygon_name))
            p <- p + geom_density(alpha = 0.4)
            p <- p + guides(color = FALSE)
            p <- p + switch(palette,
                            "contrast" = khroma::scale_color_contrast(),
                            "bright" = khroma::scale_colour_bright(),
                            "vibrant" = khroma::scale_colour_vibrant(),
                            "muted" = khroma::scale_colour_muted())
            p <- p + switch(palette,
                            "contrast" = khroma::scale_fill_contrast(),
                            "bright" = khroma::scale_fill_bright(),
                            "vibrant" = khroma::scale_fill_vibrant(),
                            "muted" = khroma::scale_fill_muted())
            p <- p + labs(y = "Density", x = NULL, fill = "Place")}

    p <- p + theme_bw()
    p <- p + theme(legend.position="bottom")

    if(use_default_palette == TRUE) {
        p <- p + theme(axis.ticks.x = element_blank(),
                       axis.text.x = element_blank())
        p <- p + theme(panel.grid.minor.x=element_blank(),
                       panel.grid.major.x=element_blank())
        p <- p + facet_wrap(plot_title ~ ., scales = "free_y")
    }
    else {
        p <- p + facet_wrap(~plot_title, ncol = 2, scales = "free")
    }

    switch(tools::file_ext(out_file),
           "pdf" = ggsave(filename = out_file,
                          plot = p,
                          height = height,
                          width = width,
                          limitsize = FALSE,
                          device = cairo_pdf),
           "png" = ggsave(filename = out_file,
                          plot = p,
                          height = height,
                          width = width,
                          dpi = png_dpi,
                          limitsize = FALSE,
                          type = "cairo"),
           ggsave(filename = out_file,
                  plot = p,
                  height = height,
                  width = width,
                  limitsize = FALSE))

}

plot_annual_metrics <- function(data = NULL,
                                  input_file_name = NULL,
                                  output_dir = getwd(),
                                  output_graphic = "annual_metrics.pdf",
                                  order_legend = FALSE,
                                  palette = "bright",
                                  default_palette = "viridis",
                                  png_dpi =120,
                                  verbose = TRUE) {

    library('ggplot2')

    palette <- check_palette(palette)
    default_palette <- check_default_palette(default_palette)
    data <- get_data(data, input_file_name)
    use_default_palette <- choose_palette(data, palette, default_palette)

    color_plot_height <- c(15, 4, 6)
    min_plot_size <- 7 / 2
    bw_plot_width <- min_plot_size * length(data$rasters)

    out_file <- file.path(output_dir, output_graphic)
    out_height <- if(use_default_palette == TRUE)
                      min_plot_size * 4
                  else
                      color_plot_height[1]

    out_width <- if(use_default_palette == TRUE)
                     min_plot_size * 5
                 else
                     7

    if(verbose == TRUE)
        message(sprintf("Writing annual metrics to %s", out_file))

    comb_dfs <- NULL
    for (i in 1:length(data$annual)) {

        ## Convert selected rasters, one for each area, into a list of data frames
        dfs <- lapply(X = data$rasters, FUN = function(x) as.data.frame(x[[i]]))

        ## add columns used for plotting and remove rows with NA data values
        ## polygon_name distinguishes areas in each of the plots
        ## plot_title distinguishes plots from one another
        for(j in 1:length(dfs)) {
            dfs[[j]]$polygon_name <- data$polygon_names[j]
            dfs[[j]]$plot_title <- names(data$rasters[[1]])[i]
            colnames(dfs[[j]])[1] <- "value"
            dfs[[j]] <- dfs[[j]][!is.na(dfs[[j]]$value), ]}

        comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

    ## Possibly order the legend labels
    if(order_legend == TRUE)
        if(!is.null(data$sort_order)) {
            temp_df <- data.frame(n = data$polygon_names,
                                  o = data$sort_order)
            ordered_names <- temp_df[order(temp_df$o),]$n
            comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                            levels = ordered_names)}
        else
            message(sprintf("Legend sort order is not specified in %s",
                            if(is.null(input_file_name))
                                "input data"
                            else
                                input_file_name))

    comparative_graph(data = comb_dfs,
                      out_file = out_file,
                      palette = palette,
                      default_palette = default_palette,
                      use_default_palette = use_default_palette,
                      height = out_height,
                      width = out_width)
    }

plot_wind_metrics <- function(data = NULL,
                              input_file_name = NULL,
                              output_dir = getwd(),
                              output_graphic = "wind_metrics.pdf",
                              order_legend = FALSE,
                              palette = "bright",
                              default_palette = "viridis",
                              png_dpi =120,
                              verbose = TRUE) {

    library('ggplot2')

    palette <- check_palette(palette)
    default_palette <- check_default_palette(default_palette)
    data <- get_data(data, input_file_name)
    use_default_palette <- choose_palette(data, palette, default_palette)

    out_file <- file.path(output_dir, output_graphic)

    color_plot_height <- c(15, 4, 6)
    min_plot_size <- 7 / 2
    bw_plot_width <- min_plot_size * length(data$rasters)

    out_height <- if(use_default_palette == TRUE)
                      7
                  else
                      color_plot_height[2]

    out_width <- if(use_default_palette == TRUE)
                     min_plot_size * 3
                 else
                     7


    if(verbose == TRUE)
        message(sprintf("Writing annual wind metrics to %s", out_file))

    comb_dfs <- NULL
    for(i in 1:nlayers(data$rasters[[1]][[data$separate[1]]])) {
        dfs <- lapply(X = data$rasters,
                      FUN = function(x)
                          as.data.frame(raster(x[[data$separate[[1]]]],
                                               layer = i)))

        ## add columns used for plotting and remove rows with NA data values
        ## polygon_name distinguishes areas in each of the plots
        ## plot_title distinguishes plots from one another
        for(j in 1:length(dfs)) {
            dfs[[j]]$polygon_name <- data$polygon_names[j]
            dfs[[j]]$plot_title <- names(data$rasters[[1]][[data$separate[[1]]]])[i]
            colnames(dfs[[j]])[1] <- "value"
            dfs[[j]] <- dfs[[j]][!is.na(dfs[[j]]$value), ]}

        comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

    ## Possibly order the legend labels
    if(order_legend == TRUE)
        if(!is.null(data$sort_order)) {
            temp_df <- data.frame(n = data$polygon_names,
                                  o = data$sort_order)
            ordered_names <- temp_df[order(temp_df$o),]$n
            comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                            levels = ordered_names)}
        else
            message(sprintf("Legend sort order is not specified in %s",
                            if(is.null(input_file_name))
                                "input data"
                            else
                                input_file_name))

    comparative_graph(data = comb_dfs,
                      palette = palette,
                      default_palette = default_palette,
                      use_default_palette = use_default_palette,
                      out_file = out_file,
                      height = out_height,
                      width = out_width)
}

plot_rainfall_metrics <- function(data = NULL,
                                input_file_name = NULL,
                                output_dir = getwd(),
                                output_graphic = "rainfall_metrics.pdf",
                                order_legend = FALSE,
                                palette = "bright",
                                default_palette = "viridis",
                                png_dpi =120,
                                verbose = TRUE) {

    library('ggplot2')

    palette <- check_palette(palette)
    default_palette <- check_default_palette(default_palette)
    data <- get_data(data, input_file_name)
    use_default_palette <- choose_palette(data, palette, default_palette)
    ## intra-annual rainfall metrics

    comb_dfs <- NULL
    out_file <- file.path(output_dir, output_graphic)

    color_plot_height <- c(15, 4, 6)
    min_plot_size <- 7 / 2
    bw_plot_width <- min_plot_size * length(data$rasters)

    out_height <- if(use_default_palette == TRUE)
                      7
                  else
                      color_plot_height[3]
    out_width <- if(use_default_palette == TRUE)
                     min_plot_size * 4
                 else
                     7


    if(verbose == TRUE)
        message(sprintf("Writing intra-annual rainfall metrics to %s",
                        out_file))

    for (i in 1:length(data$rasters)) {
        dfs <- lapply(X = data$rasters,
                      FUN = function(x) as.data.frame(x$rf.ann))

        ## add columns used for plotting and remove rows with NA data values
        ## polygon_name distinguishes areas in each of the plots
        ## plot_title distinguishes plots from one another
        for(j in 1:length(dfs)) {
            dfs[[j]]$polygon_name <- data$polygon_names[j]
            dfs[[j]]$plot_title <- names(data$rasters[[1]]$rf.ann)
            colnames(dfs[[j]])[1] <- "value"
            dfs[[j]] <- dfs[[j]][!is.na(dfs[[j]]$value), ]}

        comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

    for(i in 1:nlayers(data$rasters[[1]][[data$separate[2]]])) {
        dfs <- lapply(X = data$rasters,
                      FUN = function(x)
                          as.data.frame(raster(x[[data$separate[[2]]]],
                                               layer = i)))

        ## add columns used for plotting and remove rows with NA data values
        ## polygon_name distinguishes areas in each of the plots
        ## plot_title distinguishes plots from one another
        for(j in 1:length(dfs)) {
            dfs[[j]]$polygon_name <- data$polygon_names[j]
            dfs[[j]]$plot_title <- names(data$rasters[[1]][[data$separate[[2]]]])[i]
            colnames(dfs[[j]])[1] <- "value"
            dfs[[j]] <- dfs[[j]][!is.na(dfs[[j]]$value), ]}

        comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

    ## Possibly order the legend labels
    if(order_legend == TRUE)
        if(!is.null(data$sort_order)) {
            temp_df <- data.frame(n = data$polygon_names,
                                  o = data$sort_order)
            ordered_names <- temp_df[order(temp_df$o),]$n
            comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                            levels = ordered_names)}
        else
            message(sprintf("Legend sort order is not specified in %s",
                            if(is.null(input_file_name))
                                "input data"
                            else
                                input_file_name))

    comparative_graph(data = comb_dfs,
                      out_file = out_file,
                      palette = palette,
                      default_palette = default_palette,
                      use_default_palette = use_default_palette,
                      height = out_height,
                      width = out_width)
}

plot_annual_climate <- function(data = NULL,
                                input_file_name = NULL,
                                output_dir = getwd(),
                                output_graphic = "annual_climate.pdf",
                                order_legend = FALSE,
                                palette = "bright",
                                default_palette = "viridis",
                                png_dpi =120,
                                verbose = TRUE) {

    library('ggplot2')

    palette <- check_palette(palette)
    default_palette <- check_default_palette(default_palette)
    data <- get_data(data, input_file_name)
    use_default_palette <- choose_palette(data, palette, default_palette)

## FIGURE 2: Density plots or boxplots of mean annual climate

    ann_fin <- c(1, 13, 2, 9, 3, 4, NA, 6)

    names_ann_fin <- c("Elevation (m)",
                       "Mean Air Temperature",
                       "Annual Rainfall (mm)",
                       "Relative Humidity (%)",
                       "Cloud Frequency",
                       "Shortwave Radiation",
                       "Mean Wind Speed",
                       "Potential ET (mm)")

    ## fig_label <- letters[1:length(ann_fin)]

    comb_dfs <- NULL
    out_file <- file.path(output_dir, output_graphic)
    out_height <- 9
    out_width <- 9

    if(verbose == TRUE)
        message(sprintf("Writing mean annual climate metrics to %s",
                        out_file))

    for (i in seq(along = ann_fin)) {
        for (j in seq(along = data$rasters)) {
            dfs <- if(!is.na(ann_fin[i]))
                       lapply(X = data$rasters,
                              FUN = function(x) as.data.frame(x[[ann_fin[i]]]))
                   else
                       lapply(X = data$rasters,
                              FUN = function(x)
                                  as.data.frame(raster(x[[data$separate[1]]],
                                                       layer = 1)))

            for(k in seq(along = dfs)) {
                dfs[[k]]$polygon_name <- data$polygon_names[k]
                dfs[[k]]$plot_title <- names_ann_fin[i]
                colnames(dfs[[k]])[1] <- "value"
                dfs[[k]] <- dfs[[k]][!is.na(dfs[[k]]$value), ]}

            comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

    ## Possibly order the legend labels
    if(order_legend == TRUE)
        if(!is.null(data$sort_order)) {
            temp_df <- data.frame(n = data$polygon_names,
                                  o = data$sort_order)
            ordered_names <- temp_df[order(temp_df$o),]$n
            comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                            levels = ordered_names)}
        else
            message(sprintf("Legend sort order is not specified in %s",
                            if(is.null(input_file_name))
                                "input data"
                            else
                                input_file_name))
    }

    comparative_graph(data = comb_dfs,
                      out_file = out_file,
                      palette = palette,
                      default_palette = default_palette,
                      use_default_palette = use_default_palette,
                      height = out_height,
                      width = out_width)

} # End plot_annual_summaries()
