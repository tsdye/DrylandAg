plot_annual_summaries <- function(data = NULL,
                                  input_file_name = NULL,
                                  output_dir = getwd(),
                                  annual_summary_file = "annual_summary_file.pdf",
                                  legend_order = NULL,
                                  palette = NULL,
                                  verbose = TRUE) {

    library('ggplot2')

    ## set up
    if(is.null(data) & is.null(input_file_name))
        stop("One of 'data' and 'input_file_name' required")

    if(!is.null(data) & !is.null(input_file_name))
        stop("Only one of 'data' and 'input_file_name' required")

    if(!is.null(input_file_name))
        if(!file.exists(input_file_name))
            stop(sprintf("The input file %s does not exist", input_file_name))

    if(!is.null(input_file_name))
        data <- readRDS(file = input_file_name)

    max_colors <- c(bright = 7, contrast = 3, vibrant = 7, muted = 9)

    if(!is.null(palette))
        if(length(data$rasters) > max_colors[palette])
            stop(sprintf("Requested %i colors but %s offers only %i",
                         length(data$rasters), palette, max_colors[palette]))
        else
            message(sprintf("Preparing color output using the %s palette", palette))

    color_plot_height <- c(15, 4, 6)
    min_plot_size <- 7 / 2
    bw_plot_width <- min_plot_size * length(data$rasters)

#### Local functions
    ## set up some functions for plotting
    ## TD, deprecated?
    addHI3dryline <- function() plot(HI3dry, add=TRUE)  # show outline

    ## Utility function to generate file paths that can be used in series
    filename_splice <- function(pathname, filename, splice) {
        name_parts <- strsplit(basename(filename), ".", fixed = TRUE)
        file.path(pathname, paste0(name_parts[[1]][1], splice, ".",
                                   name_parts[[1]][2]))
    }

    ## function for plotting comparative graphs
    comparative_graph <- function(data, out_file, palette = NULL,
                                  height = 7, width = 7) {
        if(is.null(palette)) {
            p <- ggplot(data = data, aes(x = polygon_name, y = value, fill = polygon_name))
            p <- p + geom_boxplot(outlier.shape = 20, alpha = 0.4)
            p <- p + scale_fill_grey()
            ## p <- p + guides(fill = FALSE)
            p <- p + labs(y = "Value", x = NULL, fill = "Name")}
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
                p <- p + labs(y = "Density", x = NULL, fill = "Name")}
        p <- p + theme_bw()
        if(is.null(palette)) {
            p <- p + theme(axis.ticks.x = element_blank(),
                           axis.text.x = element_blank())
            p <- p + theme(panel.grid.minor.x=element_blank(),
                           panel.grid.major.x=element_blank())
            p <- p + facet_wrap(plot_title ~ ., scales = "free_y")
        }
        else {
            p <- p + facet_wrap(~plot_title, ncol = 2, scales = "free")
        }
        ggsave(filename = out_file, plot = p, height = height, width = width)
    }

    ## Plot annual metrics across areas of interest

    out_file <- filename_splice(output_dir, annual_summary_file, "_1")
    out_height <- if(is.null(palette))
                      min_plot_size * 4
                  else
                      color_plot_height[1]

    out_width <- if(is.null(palette))
                     min_plot_size * 4
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
    if(!is.null(legend_order))
        comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                        levels = legend_order)


    comparative_graph(data = comb_dfs, palette = palette,
                      out_file = out_file, height = out_height,
                      width = out_width)

    ## wind metrics
    out_file <- filename_splice(output_dir, annual_summary_file, "_2")
    out_height <- if(is.null(palette))
                      7
                  else
                      color_plot_height[2]

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
    if(!is.null(legend_order))
        comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                        levels = legend_order)

    comparative_graph(data = comb_dfs, palette = palette,
                      out_file = out_file, height = out_height)

    ## intra-annual rainfall metrics

    comb_dfs <- NULL
    out_file <- filename_splice(output_dir, annual_summary_file, "_3")
    out_height <- if(is.null(palette))
                      7
                  else
                      color_plot_height[3]
    out_width <- if(is.null(palette))
                     min_plot_size * 3
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
    if(!is.null(legend_order))
        comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                        levels = legend_order)

    comparative_graph(data = comb_dfs, palette = palette,
                      out_file = out_file, height = out_height,
                      width = out_width)}
