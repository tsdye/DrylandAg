plot_monthly_summaries <- function(data = NULL,
                                   input_file_name = NULL,
                                   output_dir = getwd(),
                                   monthly_summary_file = "monthly_summary_file.pdf",
                                   order_legend = FALSE,
                                   palette = "bright",
                                   png_dpi = 120,
                                   verbose = TRUE) {

    library(ggplot2)

    ## set up
    if(is.null(data) & is.null(input_file_name))
        stop("One of 'data' and 'input_file_name' required")

    if(!is.null(data) & !is.null(input_file_name))
        stop("Only one of 'data' and 'input_file_name' required")

    if(!is.null(input_file_name))
        if(!file.exists(input_file_name))
            stop(sprintf("The input file '%s' does not exist", input_file_name))

    if(!is.null(input_file_name))
        data <- readRDS(file = input_file_name)

    max_colors <- c(bright = 7, contrast = 3, vibrant = 7, muted = 9)

    if(length(data$rasters) > max_colors[palette])
        use_grey <- TRUE
    else
        use_grey <- FALSE

    if (use_grey == TRUE)
        message(sprintf("Requested %i colors but %s palette offers %i. Using default palette.",
                        length(data$rasters), palette, max_colors[palette]))
    else
        message(sprintf("Preparing color output using the %s palette", palette))

    min_plot_size = 7/3
    monthly_plot_size = 15 * min_plot_size

    out_file <- file.path(output_dir, monthly_summary_file)

    comb_dfs <- NULL
    ## make data frame
    for (i in data$monthly) {

        ## Convert selected rasters, one for each area,
        ## into a list of data frames
        dfs <- lapply(X = data$rasters, FUN = function(x)
            as.data.frame(x[[i]]))

        ## add columns used for plotting and remove rows with NA data values
        ## polygon_name distinguishes areas in each of the plots
        ## facet distinguishes plots from one another
        for(j in 1:length(dfs)) {
            names(dfs[[j]]) <- month.abb
            dfs[[j]]$polygon_name <- data$polygon_names[j]
            dfs[[j]]$facet <- names(data$rasters[[1]])[i]
            dfs[[j]] <- suppressMessages(reshape2::melt(dfs[[j]],
                                                        na.rm = TRUE))
        }

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

    if(verbose == TRUE) message(sprintf("Writing monthly metrics to %s",
                                        out_file))

    ## color boxplots
    if(use_grey == FALSE)
        p <- ggplot(data = comb_dfs, aes(x = variable, y = value,
                                         fill = polygon_name,
                                         color = polygon_name))
    else
        p <- ggplot(data = comb_dfs,
                    aes(x = variable, y = value, fill = polygon_name))

    p <- p + theme_bw()
    p <- p + theme(legend.position="bottom")
    p <- p + labs(x = "Month", y = NULL, fill = "Place")

    if(use_grey == FALSE) {
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
                        "muted" = khroma::scale_fill_muted()) }
    else {
           ## p <- p + khroma::scale_fill_sunset()
           p <- p + guides(fill = guide_legend(nrow = 2))}

    p <- p + geom_boxplot(outlier.shape = 20, alpha = 0.4)
    p <- p + facet_grid(facet ~ ., scales = "free_y")

    switch(tools::file_ext(out_file),
           "pdf" = ggsave(filename = out_file,
                          plot = p,
                          height = monthly_plot_size,
                          width = monthly_plot_size,
                          limitsize = FALSE,
                          device = cairo_pdf),
           "png" = ggsave(filename = out_file,
                          plot = p,
                          height = monthly_plot_size,
                          width = monthly_plot_size,
                          dpi = png_dpi,
                          limitsize = FALSE,
                          type = "cairo"),
           ggsave(filename = out_file,
                  plot = p,
                  height = monthly_plot_size,
                  width = monthly_plot_size,
                  limitsize = FALSE))

    if(verbose == TRUE) message("Pau")
    comb_dfs } # end of function
