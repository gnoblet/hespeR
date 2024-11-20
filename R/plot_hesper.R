## Graphing functions for HESPER
# produce histogram for numerical variables, with facet (intended for HESPER, could work with other numerical)
options(scipen = 200000)

## Define below the plot.hist function arguments as for a package
#' @param df data frame
#' @param var variable to plot
#' @param fill variable to facet
#' @param title title of the plot
#' @param subtitle subtitle of the plot
#' @param bins.val number of bins
#' @param median whether to plot median or mean
#' @param scale_density scale density
#' @param smoothness smoothness of the density
#' @return ggplot object
#' 

plot.hist <- function(df, 
                      var="nb_hesper.all",
                      fill=NULL,
                      title=str_replace_all(var, "_|\\.", " "),
                      subtitle = NULL, bins.val=10, 
                      median=T, scale_density=2, smoothness=.5){
  # df_filtered <- df %>% select(any_of(c(var, fill))) %>% pivot_longer(any_of(c(var))) %>% mutate(value=as.numeric(value))
  df_filtered <- df %>% select(any_of(c(var, fill))) %>% pivot_longer(any_of(c(var))) %>% 
    mutate(value=as.numeric(value)) %>% group_by(!!!syms(fill)) %>% 
    mutate(median_val = median(value, na.rm = TRUE), mean_val = mean(value, na.rm = TRUE))
  
  plot <- df_filtered %>% ggplot(aes(x=value, fill="#EE5859")) + 
    geom_histogram(aes(y = after_stat(width * density)), show.legend = FALSE, bins = bins.val) +
    scale_x_continuous() + scale_y_continuous(labels=scales::percent_format()) +
    geom_density(aes(y = after_stat(density*scale_density)), fill="#464647", alpha=.2, bw=smoothness) +
    labs(x="", y="% of respondents", title=title, subtitle=subtitle, caption = paste0("", sum(!is.na(df[[var]])), " out of ", length(df[[var]]), " respondents.")) +
    theme_minimal() + theme(plot.title = element_text(), plot.subtitle = element_text(size=9))
  if (!is.null(fill)) plot <- plot + facet_grid(rows = vars(!!sym(fill)), scales = "free_x")
  if (median) {
    plot <- plot +
      geom_vline(aes(xintercept=median_val), col="#0067A9", linewidth=1) +
      geom_text(aes(label=paste0("median: ", round(median_val, 1)), y=0, x=median_val), vjust=-1, hjust=-0.1, col="#0067A9", size=3)
  } else {
    plot <- plot + 
      geom_vline(aes(xintercept=mean_val), col="#0067A9", linewidth=1) +
      geom_text(aes(label=paste0("average: ", round(mean_val, 1)), y=0, x=mean_val), vjust=-1, hjust=-0.1, col="#0067A9", size=3)
  }
  plot
  return(plot)
}

## Define below the plot.num function arguments as for a package
#' @param df data frame
#' @param col_group column name of the variable used to split/facet the graph
#' @param col_stat column name of the numeric variable to plot
#' @param col_n column name of the number of observations
#' @param title title of the plot
#' @param subtitle subtitle of the plot
#' @param width width of the plot
#' @param height height of the plot
#' @param dir directory to save the plot
#' @param add_name additional name pattern for saved file for the group variable
#' @param save binary determining whether to save the plot
#' 

plot.num <- function(df = median_nb_serious,
                     col_group = "pop_group", col_stat = "stat", col_n="n_unw", 
                     title = "Average number of serious problems by HH's residence status", subtitle="",
                     width=NULL, height=NULL, dir = "exploratory/graph/number_problems_", add_name ="",
                     save=T) {
  
  plot <- df %>%
    mutate(!!sym(col_group) := paste0(!!sym(col_group), "\nn=", unique(!!sym(col_n)))) %>%
    ggplot(aes(x=reorder(!!sym(col_group), !!sym(col_stat)), y=!!sym(col_stat))) + 
    geom_point(stat="identity", show.legend = F, color="#EE5859") +
    geom_segment(aes(xend = reorder(!!sym(col_group), -!!sym(col_stat)), yend = 0), color="#EE5859") +
    geom_text(aes(label=round(!!sym(col_stat),1)), hjust=-1, size=3) +
    scale_x_discrete(label=~str_replace_all(., "_", " ")) +
    theme_minimal() + theme(plot.title=element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + 
    labs(title=title, subtitle=subtitle, fill="", x="", y="") + coord_flip()
  
  if (is.null(width)) width <- 8
  if (is.null(height)) height <- .5 + length(unique(df[[col_group]]))*2/3
  
  if (save) ggsave(paste0(dir, "by_", add_name, ".png"), plot, bg="white", height=height, width=width)
  return(plot)
}

## Define below the create_graph_facet_disag function arguments as for a package
#' @param df data frame
#' @param indicator indicator to plot
#' @param col_disag disaggregation variable
#' @param flip binary to flip the graph
#' @param fwidth width of the plot
#' @param figheight height of the plot
#' @param custom.factor binary to wrap the choice labels
#' @param error_bar binary to add error bars
#' @param text_vpos vertical position of the geom_text displaying the percentage value
#' @param add.ggplot additional ggplot2 code
#' @param custom.folder custom folder to save the plot
#' @param custom.file custom file name to save the plot
#' 
create_graph_facet_disag <- function(df=result_hesper, 
                                     indicator="at_least_one_hesper_item_section", 
                                     col_disag="", flip=T,
                                     fwidth=12, 
                                     figheight=NULL, 
                                     custom.factor=F,
                                     error_bar=T,
                                     text_vpos=0.035,
                                     add.ggplot=NULL, 
                                     custom.folder=NULL,
                                     custom.file=NULL) {
  
  df_filtered <- df %>% filter(question==indicator)
  if (!custom.factor) {df_filtered <- df_filtered %>% mutate(choice_label = str_wrap(choice_label,60))}
  
  choices <- unique(df_filtered$choice_label)
  min_height <- 5  # Adjust as needed
  max_height <- 40  # Adjust as needed
  fig.height <- (1.5 + .1 * length(choices)) * length(col_disag)
  if (length(col_disag) > 2) fig.height <- (1.5 + .4 * length(choices)*2) * length(unique(df_filtered$df_filteredset)) * length(col_disag) / 2 else 
    fig.height <- pmax(min_height, pmin(max_height, fig.height))
  
  if (!is.null(figheight)) fig.height <- figheight
  x_aesthetic <- if (!custom.factor) {reorder(df_filtered$choice_label, df_filtered$mean)} else {df_filtered$choice_label}
  if (col_disag=="") {
    p <- ggplot(df_filtered, aes(x = x_aesthetic, y = mean)) + geom_bar(fill = "#0067A9", stat = "identity")
  } else {p <- ggplot(df_filtered, aes(x = x_aesthetic, y = mean, fill = !!sym(col_disag))) + geom_bar(position = "dodge", stat = "identity")}
  
  p <- p + 
    geom_text(aes(y = mean + 1.96 * sqrt(mean * (1 - mean) / n) + text_vpos, label = ifelse(mean != 0, paste0(round(100 * mean, 1), "%"), "")), position = position_dodge(width = 0.9), size = 2.75) +
    scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.2), limits = c(0, 1.2)) +
    labs(title = paste(unique(df_filtered$ind_label)), x = "", y = "% of respondents", fill=str_replace_all(col_disag, "_", " "), subtitle = "") +
    scale_fill_manual(values = c("#0067A9", "#EE5859", "#C7C8CA", "#D2CBB8","#84A181", "#58585A")) + theme_minimal() +  # Change colors as needed
    theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), legend.position = "bottom") 
  
  if (flip) p <- p + coord_flip()

  ## calculate confidence interval using unweighted standard errors if not present, otherwise use mean/low and mean/upp from srvyr
  if(error_bar){
    if (!sum(str_detect(colnames(df_filtered), "mean/(upp|low)"))==2) {
      p <- p + geom_errorbar(aes(ymin = mean - 1.96 * sqrt(mean * (1 - mean) / n), ymax = mean + 1.96 * sqrt(mean * (1 - mean) / n)),
                             color="#58585A", width = 0.3, position = position_dodge(width = 0.9))
    } else {
      p <- p + geom_errorbar(aes(ymin = `mean/low`, ymax = `mean/upp`), color="#58585A", width = 0.3, position = position_dodge(width = 0.9))}
  }
  if (!is.null(add.ggplot)) p <- p + add.ggplot
  
  if (is.null(custom.folder)) folder <- "graph/category/" else folder <- custom.folder
  if (!is.null(custom.file)) {ggsave(paste0(folder, custom.file, ".png"), p, bg = "white", height = fig.height, width = fwidth)} else {
    ggsave(paste0(folder, indicator, "_", col_disag, ".png"), p, bg = "white", height = fig.height, width = fwidth)} 
  
  return(p)
}
