if (!require(tidyverse))
  install.packages('tidyverse')
library(tidyverse)

library(ggplot2)
library(reshape2) # for using melt
library(dplyr)
library(ggthemes)
library(wesanderson)
library(svglite)
library(plotly)

# Colorblind-friendly palette (source: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/)
# The palette with black:
cbp <- c(
  "#000000",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)


#-------------------------------------------------------------------------------

# Make plots of fitness averages
# save = TRUE is for writing plots as svg images for the manuscript
# save = FALSE is for generating and showing plots in the online notebook
plot_fitness_averages <- function(map_vector, fitness_filename, save) {
  # 30 seeds + 2 columns for method and step number
  limit = 32
  
  # Read csv with fitness for every method, step and seed
  fitness_all_seeds = read_delim(
    fitness_filename,
    col_names = TRUE,
    show_col_types = FALSE,
    skip = 1,
    delim = ','
  )
  
  # There are some missing values in previous results. Replace zeros with NA
  fitness_all_seeds <-
    fitness_all_seeds %>% mutate(across(3:limit,  ~ na_if(., 0)))
  
  # Convert to factor and define order of levels
  # Makes it easier to use the palette as intended for plotting
  fitness_all_seeds$method <-
    factor(fitness_all_seeds$method,
           levels = c("ESS-NS", "ESSIM-EA", "ESSIM-DE", "ESS"))
  
  # Compute mean and standard deviation. Omit NAs
  fitness_all_seeds_mean <- fitness_all_seeds  %>%
    rowwise() %>%
    mutate(m = mean(c_across(3:limit), na.rm = TRUE),
           sd = sd(c_across(3:limit), na.rm = TRUE))
  
  # Plot average fitness
  p <-
    ggplot(data = fitness_all_seeds_mean, aes(
      group = method,
      colour = method,
      shape = method
    )) +
    # ESS-NS
    geom_line(data = fitness_all_seeds_mean, aes(x = step, y = m),
              size = 1.5) +                 # thickness of line
    geom_point(data = fitness_all_seeds_mean, aes(x = step, y = m),
               size = 4)    +                 # size of points
    #geom_errorbar(aes(x=step,ymin=m-sd, ymax=m+sd,xmin=min(step),xmax=max(step)), width=.2) +
    labs(
      color = 'Method',
      shape = 'Method',
      alt = paste(
        "A plot showing the average fitness (y axis) over 30 repetitions at each prediction step (x axis) for each method (shown in different color and shape)"
      )
    ) +
    #scale_color_brewer(palette="Dark2") +  # colors
    scale_color_manual(values = cbp) +      # other palette
    ylab("average fitness") +               # y axis label
    #ylim(0.0,1) +
    scale_x_discrete(name = "prediction step",
                     limits = unique(as.vector(fitness_all_seeds_mean$step))) +  # x ticks
    scale_y_continuous(limits = c(0.25, 1), breaks = seq(0, 1, 0.1)) +
    #theme_calc() +
    theme(
      text = element_text(size = 16),
      panel.grid.minor = element_line(
        colour = "lightgray",
        linetype = "solid",
        size = 0.3
      ),
      panel.grid.major = element_line(
        colour = "lightgray",
        linetype = "solid",
        size = 0.6
      )
    ) +
    #panel.border = element_rect(colour="black", size=0.75)) +
    ggtitle(mapn)                           # for the main title
  
  # Save or print image
  if (save) {
    ggsave(
      paste(
        c("../figures/fitness_averages_", mapn, ".svg"),
        collapse = ""
      ),
      plot = p,
      height = 4.5,
      units = "in"
    )
  } else {
    #print(p)
    print(htmltools::tagList(ggplotly(p)))
  }
  
}

#-------------------------------------------------------------------------------
# Plot fitness distribution
# save = TRUE is for writing plots as svg images for the manuscript
# save = FALSE is for generating and showing plots in the online notebook
# jitter = TRUE adds points to boxplot, shows more information but may be
# cumbersome in print
plot_fitness_distribution <- function(map_vector,
                                      fitness_filename,
                                      save,
                                      jitter) {
  # 30 seeds + 2 columns for method and step number
  limit = 32
  
  # Read csv with fitness for every method, step and seed
  fitness_all_seeds = read_delim(
    fitness_filename,
    col_names = TRUE,
    show_col_types = FALSE,
    skip = 1,
    delim = ','
  )
  
  # Replace missing values (coded as 0 in the csv) by NAs
  fitness_all_seeds <-
    fitness_all_seeds %>% mutate(across(3:limit,  ~ na_if(., 0)))
  
  # Convert to factor and define order of levels
  # Makes it easier to use the palette as intended for plotting
  fitness_all_seeds$method <-
    factor(fitness_all_seeds$method,
           levels = c("ESS-NS", "ESSIM-EA", "ESSIM-DE", "ESS"))
  
  df <- gather(fitness_all_seeds, "seed", "fitness", 3:limit) %>%
    select(-seed) # TODO maybe this could have been removed in the previous line
  
  # Plot, omit NAs.
  p <- ggplot(df %>% filter(!is.na(method)),
              aes(
                x = method,
                y = fitness,
                color = method,
                group = method
              )) +
    facet_wrap( ~ step, nrow = 1) + # one column per prediction step
    scale_color_manual(values = cbp) +
    theme(
      text = element_text(size = 20),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 10,
        color = cbp
      ),
      axis.title.x = element_text(
        margin = margin(t = 20)
      )
    ) +
    guides(color = FALSE) +
    ggtitle(mapn)  # for the main title
  
  if (jitter) {
    p <-
      p + geom_boxplot(outlier.shape = NA) +  # avoid duplicating the outliers in boxplot
      geom_jitter()
  } else {
    p <- p + geom_boxplot()
  }
  
  # Save or print image
  if (save) {
    ggsave(
      paste(
        c("../figures/fitness_distribution_", mapn, ".svg"),
        collapse = ""
      ),
      plot = p,
      height = 4.5,
      units = "in"
    )
  } else {
    print(p)
  }
}



#-------------------------------------------------------------------------------
#
calibration_heatmap <- function(map_vector, format) {
  #compute_calibration_averages <- function(map_vector){
  # Source: https://sebastianraschka.com/Articles/heatmaps_in_r.html
  if (!require("gplots")) {
    install.packages("gplots", dependencies = TRUE)
    library(gplots)
  }
  if (!require("RColorBrewer")) {
    install.packages("RColorBrewer", dependencies = TRUE)
    library(RColorBrewer)
  }
  library(ztable) # https://cran.r-project.org/web/packages/ztable/vignettes/heatmapTable.html
  library(magrittr)
  
  #
  
  tournament_parameters = c("0.75", "0.8", "0.85", "0.9")
  mutation_parameters = c("0.1", "0.2", "0.3", "0.4")
  
  n_configurations = length(tournament_parameters) * length(mutation_parameters)
  
  configurations <- c()
  averages_list <- list()
  
  # 751, 520, 533, 519, 534
  steps = matrix(nrow = 5, ncol = 2)
  steps[, 1] = c(751, 520, 533, 519, 534)
  steps[, 2] = c(3, 5, 4, 3, 5)
  
  for (mapn in map_vector) {
    i = 1
    n_step = steps[steps[, 1] == mapn, 2]
    
    averages <-
      as.data.frame(matrix(nrow = n_configurations, ncol = n_step + 1))
    colnames(averages) = c(1:n_step, "t (s)")
    
    
    for (tour in tournament_parameters) {
      for (mut in mutation_parameters) {
        csvname = c(
          "../data/calibration_results/",
          mapn,
          "/all_fitness_",
          mapn,
          "_TOUR",
          tour,
          "_MUT",
          mut,
          ".csv"
        )
        
        fitness_ns = read_csv(
          paste(csvname, collapse = ""),
          col_names = FALSE,
          show_col_types = FALSE
        )
        
        fitness_ns <- fitness_ns %>%
          filter(row_number() != 1)
        
        # compute mean rowwise
        fitness_ns <-
          fitness_ns %>% rowwise() %>% mutate(m = mean(c_across(X1:X10)), sd = sd(c_across(X1:X10)))
        
        # TODO I should change file name in bash script process_results.sh
        timesname = c(
          "../data/calibration_results/",
          mapn,
          "/avg_execution_time_",
          tour,
          "_MUT",
          mut,
          ".csv"
        )
        runtime = read_csv(
          paste(timesname, collapse = ""),
          col_names = FALSE,
          show_col_types = FALSE
        )
        
        conf_name <- paste(tour, mut, sep = ", ")
        configurations[i] <- conf_name
        #print(conf_name) # for debugging
        
        # store fitness average for this configuration
        averages[i, ] = c(t(fitness_ns$X1), runtime$X2)
        
        i = i + 1
      }
    }
    
    # Naming rows for the table
    rownames(averages) <- configurations
    
    # store the results in the list
    averages_list[[match(mapn, steps[, 1])]] <- averages
  }
  
  # HEATMAP
  options(ztable.type = format)
  maps <- map_vector
  means <- matrix(nrow = n_configurations, ncol = 5)
  colnames(means) <- maps
  i = 1
  
  for (mapn in maps) {
    tabletext <-
      paste("Fitness averages and runtimes (in seconds) for map ",
            mapn,
            ".",
            sep = "")
    
    n_step <- steps[steps[, 1] == mapn, 2]
    data <- averages_list[[match(mapn, steps[, 1])]]
    data <- data %>%
      rowwise %>%
      mutate(m = rowMeans(across(1:n_step))) %>%
      relocate(m, .before = "t (s)")
    
    means[, i] <- t(data$m)
    i = i + 1
    
    data <- as.data.frame(data)
    row.names(data) <- configurations
    
    # Invert palette so that darker cells are better for runtimes too
    runtimes_palette <- rev(brewer.pal(9, "OrRd"))
    
    output_all <- ztable((data), digits = 3) %>%
      makeHeatmap(palette = "Blues",
                  cols = c(1:n_step),
                  margin = 0) %>% # for each step
      makeHeatmap(palette = "Greens",
                  cols = n_step + 1,
                  margin = 0) %>% # average over all steps
      makeHeatmap(
        mycolor = runtimes_palette,
        reverse = TRUE,
        cols = n_step + 2,
        margin = 0
      ) %>% # avg runtime
      print(caption = tabletext)
    
    if (format == "latex") {
      # TODO save in tex file
      out_latex <- knitr::asis_output(output_all)
      knitr::asis_output(output_all)
    } else if (format == "html") {
      knitr::asis_output(htmltools::htmlPreserve(output_all))
    }
  }
  
  return(list(means, configurations))
}

#-------------------------------------------------------------------------------
# Plot runtime distribution for ESS-NS, adding average runtime values for
# competitors
# requires: calibration-compute-averages
plot_runtime_distribution <- function(map_vector, save) {
  library(scales)
  
  competitors <-
    read_csv(
      "../data/runtimes_competitors.csv",
      col_names = TRUE,
      col_types = list(col_character(), col_guess(), col_guess(), col_guess())
    )
  
  competitors <- competitors %>% mutate(map = factor(map))
  
  tournament_parameters = c("0.75", "0.8", "0.85", "0.9")
  mutation_parameters = c("0.1", "0.2", "0.3", "0.4")
  
  n_configurations = length(tournament_parameters) * length(mutation_parameters)
  data <-
    tibble(map = factor(),
           t = numeric(),
           h = vctrs::new_duration(1))
  
  for (mapn in map_vector) {
    for (tour in tournament_parameters) {
      for (mut in mutation_parameters) {
        timesname = c(
          "../data/calibration_results/",
          mapn,
          "/avg_execution_time_",
          tour,
          "_MUT",
          mut,
          ".csv"
        )
        runtime = read_csv(
          paste(timesname, collapse = ""),
          col_names = c("map", "t", "h"),
          col_types = list(col_character(), col_double(), col_guess())
        )
        runtime <- runtime %>% mutate(map = factor(map))
        data <- add_row(data, runtime)
      }
    }
  }
  
  
  #
  competitors <- competitors %>% group_by(map)
  p <- ggplot(data,
              aes(
                x = map,
                y = h,
                color = cbp[1],
                group = map
              )) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(color = 'black') +
    geom_point(
      data = competitors,
      aes(y = `ESS`, color = cbp[4]),
      shape = 16,
      size = 4
    ) +
    geom_point(
      data = competitors,
      aes(y = `ESSIM-EA`, color = cbp[3]),
      shape = 15,
      size = 4
    ) +
    geom_point(
      data = competitors,
      aes(y = `ESSIM-DE`, color = cbp[2]),
      shape = 17,
      size = 4
    ) +
    scale_color_manual(
      'Method',
      values = c('black', cbp[4], cbp[3], cbp[2]),
      label = c("ESS-NS", "ESS", "ESSIM-EA", "ESSIM-DE")
    ) + 
    scale_y_time(breaks = date_breaks('30 min'), labels = date_format('%H:%M')) +
    theme(text = element_text(size = 20)) +
    ggtitle("Runtime distribution for ESS-NS")
  
  # Save or print image
  if (save) {
    ggsave("../figures/calibration_runtime_distribution.svg",
           plot = p)
  } else {
    print(p)
  }
}
