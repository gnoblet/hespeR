# ## Graphing functions for HESPER
# # produce histogram for numerical variables, with facet (intended for HESPER, could work with other numerical)
# options(scipen = 200000)

# ## Define below the plot.hist function arguments as for a package
# #' @param df data frame
# #' @param var variable to plot
# #' @param fill variable to facet
# #' @param title title of the plot
# #' @param subtitle subtitle of the plot
# #' @param bins.val number of bins
# #' @param median whether to plot median or mean
# #' @param ggplot2::scale_density scale density
# #' @param smoothness smoothness of the density
# #' @return ggplot object
# #'

# plot_hist <- function(df,
#                       var="nb_hesper.all",
#                       fill=NULL,
#                       title=stringr::str_replace_all(var, "_|\\.", " "),
#                       subtitle = NULL, bins.val=10,
#                       median=T, scale_density=2, smoothness=.5){
#   # df_filtered <- df %>% select(any_of(c(var, fill))) %>% pivot_longer(any_of(c(var))) %>% dplyr::mutate(value=as.numeric(value))
#   df_filtered <- df |> dplyr::select(dplyr::any_of(c(var, fill))) %>% tidyr::pivot_longer(any_of(c(var))) |>
#     dplyr::mutate(value=as.numeric(value)) %>% dplyr::group_by(!!!rlang::syms(fill)) |>
#     dplyr::mutate(median_val = median(value, na.rm = TRUE), mean_val = mean(value, na.rm = TRUE))

#   plot <- df_filtered %>% ggplot2::ggplot(ggplot2::aes(x=value, fill="#EE5859")) +
#     ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(width * density)), show.legend = FALSE, bins = bins.val) +
#     ggplot2::scale_x_continuous() + ggplot2::scale_y_continuous(labels=scales::percent_format()) +
#     ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(density*ggplot2::scale_density)), fill="#464647", alpha=.2, bw=smoothness) +
#     ggplot2::labs(x="", y="% of respondents", title=title, subtitle=subtitle, caption = paste0("", sum(!is.na(df[[var]])), " out of ", length(df[[var]]), " respondents.")) +
#     ggplot2::theme_minimal() + ggplot2::theme(plot.title = ggplot2::element_text(), plot.subtitle = ggplot2::element_text(size=9))
#   if (!is.null(fill)) plot <- plot + ggplot2::facet_grid(rows = vars(!!rlang::sym(fill)), scales = "free_x")
#   if (median) {
#     plot <- plot +
#       ggplot2::geom_vline(ggplot2::aes(xintercept=median_val), col="#0067A9", linewidth=1) +
#       ggplot2::geom_text(ggplot2::aes(label=paste0("median: ", round(median_val, 1)), y=0, x=median_val), vjust=-1, hjust=-0.1, col="#0067A9", size=3)
#   } else {
#     plot <- plot +
#       ggplot2::geom_vline(ggplot2::aes(xintercept=mean_val), col="#0067A9", linewidth=1) +
#       ggplot2::geom_text(ggplot2::aes(label=paste0("average: ", round(mean_val, 1)), y=0, x=mean_val), vjust=-1, hjust=-0.1, col="#0067A9", size=3)
#   }
#   plot
#   return(plot)
# }

# ## Define below the plot.num function arguments as for a package
# #' @param df data frame
# #' @param col_group column name of the variable used to split/facet the graph
# #' @param col_stat column name of the numeric variable to plot
# #' @param col_n column name of the number of observations
# #' @param title title of the plot
# #' @param subtitle subtitle of the plot
# #' @param width width of the plot
# #' @param height height of the plot
# #' @param dir directory to save the plot
# #' @param add_name additional name pattern for saved file for the group variable
# #' @param save binary determining whether to save the plot
# #'

# plot_num <- function(df = median_nb_serious,
#                      col_group = "pop_group", col_stat = "stat", col_n="n_unw",
#                      title = "Average number of serious problems by HH's residence status", subtitle="",
#                      width=NULL, height=NULL, dir = "exploratory/graph/number_problems_", add_name ="",
#                      save=T) {

#   plot <- df |>
#     dplyr::mutate(!!rlang::sym(col_group) := paste0(!!rlang::sym(col_group), "\nn=", unique(!!rlang::sym(col_n)))) |>
#     ggplot2::ggplot(ggplot2::aes(x=reorder(!!rlang::sym(col_group), !!rlang::sym(col_stat)), y=!!rlang::sym(col_stat))) +
#     ggplot2::geom_point(stat="identity", show.legend = F, color="#EE5859") +
#     ggplot2::geom_segment(ggplot2::aes(xend = reorder(!!rlang::sym(col_group), -!!rlang::sym(col_stat)), yend = 0), color="#EE5859") +
#     ggplot2::geom_text(ggplot2::aes(label=round(!!rlang::sym(col_stat),1)), hjust=-1, size=3) +
#     ggplot2::scale_x_discrete(label=~stringr::str_replace_all(., "_", " ")) +
#     ggplot2::theme_minimal() + ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5), plot.subtitle = ggplot2::element_text(hjust=0.5)) +
#     ggplot2::labs(title=title, subtitle=subtitle, fill="", x="", y="") + ggplot2::coord_flip()

#   if (is.null(width)) width <- 8
#   if (is.null(height)) height <- .5 + length(unique(df[[col_group]]))*2/3

#   if (save) ggplot2::ggsave(paste0(dir, "by_", add_name, ".png"), plot, bg="white", height=height, width=width)
#   return(plot)
# }

# #' Define below the create_graph_ggplot2::facet_disag function arguments as for a package
# #'
# #' @param df data frame
# #' @param indicator indicator to plot
# #' @param col_disag disaggregation variable
# #' @param flip binary to flip the graph
# #' @param fwidth width of the plot
# #' @param figheight height of the plot
# #' @param custom.factor binary to wrap the choice labels
# #' @param error_bar binary to add error bars
# #' @param text_vpos vertical position of the ggplot2::geom_text displaying the percentage value
# #' @param add.ggplot additional ggplot2 code
# #' @param custom.folder custom folder to save the plot
# #' @param custom.file custom file name to save the plot
# #'
# #' @export
# facet_disag <- function(df=result_hesper,
#                                      indicator="at_least_one_hesper_item_section",
#                                      col_disag="", flip=T,
#                                      fwidth=12,
#                                      figheight=NULL,
#                                      custom.factor=F,
#                                      error_bar=T,
#                                      text_vpos=0.035,
#                                      add.ggplot=NULL,
#                                      custom.folder=NULL,
#                                      custom.file=NULL) {

#   df_filtered <- df |>dplyr::filter(question==indicator)
#   if (!custom.factor) {df_filtered <- df_filtered %>% dplyr::mutate(choice_label = stringr::str_wrap(choice_label,60))}

#   choices <- unique(df_filtered$choice_label)
#   min_height <- 5  # Adjust as needed
#   max_height <- 40  # Adjust as needed
#   fig.height <- (1.5 + .1 * length(choices)) * length(col_disag)
#   if (length(col_disag) > 2) fig.height <- (1.5 + .4 * length(choices)*2) * length(unique(df_filtered$df_filteredset)) * length(col_disag) / 2 else
#     fig.height <- pmax(min_height, pmin(max_height, fig.height))

#   if (!is.null(figheight)) fig.height <- figheight
#   x_aesthetic <- if (!custom.factor) {reorder(df_filtered$choice_label, df_filtered$mean)} else {df_filtered$choice_label}
#   if (col_disag=="") {
#     p <- ggplot2::ggplot(df_filtered, ggplot2::aes(x = x_aesthetic, y = mean)) + ggplot2::geom_bar(fill = "#0067A9", stat = "identity")
#   } else {p <- ggplot2::ggplot(df_filtered, ggplot2::aes(x = x_aesthetic, y = mean, fill = !!rlang::sym(col_disag))) + ggplot2::geom_bar(position = "dodge", stat = "identity")}

#   p <- p +
#     ggplot2::geom_text(ggplot2::aes(y = mean + 1.96 * sqrt(mean * (1 - mean) / n) + text_vpos, label = ifelse(mean != 0, paste0(round(100 * mean, 1), "%"), "")), position = ggplot2::position_dodge(width = 0.9), size = 2.75) +
#     ggplot2::scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), limits = c(0, 1.2)) +
#     ggplot2::labs(title = paste(unique(df_filtered$ind_label)), x = "", y = "% of respondents", fill=stringr::str_replace_all(col_disag, "_", " "), subtitle = "") +
#     ggplot2::scale_fill_manual(values = c("#0067A9", "#EE5859", "#C7C8CA", "#D2CBB8","#84A181", "#58585A")) + ggplot2::theme_minimal() +  # Change colors as needed
#     ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5), plot.subtitle = ggplot2::element_text(hjust=0.5), legend.position = "bottom")

#   if (flip) p <- p + ggplot2::coord_flip()

#   ## calculate confidence interval using unweighted standard errors if not present, otherwise use mean/low and mean/upp from srvyr
#   if(error_bar){
#     if (!sum(stringr::str_detect(colnames(df_filtered), "mean/(upp|low)"))==2) {
#       p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - 1.96 * sqrt(mean * (1 - mean) / n), ymax = mean + 1.96 * sqrt(mean * (1 - mean) / n)),
#                              color="#58585A", width = 0.3, position = ggplot2::position_dodge(width = 0.9))
#     } else {
#       p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = `mean/low`, ymax = `mean/upp`), color="#58585A", width = 0.3, position = ggplot2::position_dodge(width = 0.9))}
#   }
#   if (!is.null(add.ggplot)) p <- p + add.ggplot

#   if (is.null(custom.folder)) folder <- "graph/category/" else folder <- custom.folder
#   if (!is.null(custom.file)) {ggplot2::ggsave(paste0(folder, custom.file, ".png"), p, bg = "white", height = fig.height, width = fwidth)} else {
#     ggplot2::ggsave(paste0(folder, indicator, "_", col_disag, ".png"), p, bg = "white", height = fig.height, width = fwidth)}

#   return(p)
# }
