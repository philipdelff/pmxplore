#' @title Dose proportionality test (linear model)
#' @description lm fit to test for dose proportionality
#' @param df data frame
#' @param x x variable (e.g. dose)
#' @param y y parameter (e.g. auc)
#' @param ci.level confidence level for lm fit in plot, Default: 0.95
#' @param plot.par add table with lm parameters, Default: F
#' @param x_min lower limit passed to coord_carteesian (absolute), Default: 0
#' @param max_position upper y limit passed to coord_carteesian (relative max(y)), Default: 1.2
#' @param min_tab_position lower y-value for parameter table (relative max(y)), Default: 1.05
#' @param min_p_position position of p-value string (relative max(y)), Default: 0.9
#' @param signif number of significant numbers in parameter table, Default: 3
#' @param p_value_threshold p-value threshold for inclusion of intercept, Default: 0.05
#' @return list with plot, lm summary and lm fit
#' @rdname dose_proportionality_linear
#' @export 
#' @importFrom gridExtra tableGrob
#' @importFrom rlang enexpr quo eval_tidy expr_text
#' @importFrom tibble rownames_to_column

dose_proportionality_linear <- 
  function(df, x, y, ci.level=0.95, 
           plot.par=F, 
           x_min = 0, 
           max_position = 1.20,
           min_tab_position = 1.05,
           min_p_position = 0.90, 
           signif=3,
           p_value_threshold=0.05){
    
    x <- rlang::enexpr(x)
    y <- rlang::enexpr(y)
    
    # Check that x and y is in dataset
    if(!rlang::expr_text(x) %in% names(df)){
      stop(paste0(rlang::expr_text(x), " not found in dataset"))
    }
    if(!rlang::expr_text(y) %in% names(df)){
      stop(paste0(rlang::expr_text(y), " not found in dataset"))
    }
    
    # 1. Add weights to be used in fit
    newdf <- 
      rlang::quo(df %>% 
                   group_by(!!x) %>% 
                   mutate(wts = 1/var(!!y, na.rm=T))
      )
    
    newdf <- as.data.frame(rlang::eval_tidy(newdf))
    
    # Do a linear lm fit to check if intercept should be included or not
    lm_fit <- rlang::eval_tidy(
      rlang::quo(
        lm(formula=!!y~!!x, data=newdf, weights=wts)
      )
    )
    lm_sum <- summary(lm_fit)
    p_value_intercept <- signif(lm_sum$coeff[1,'Pr(>|t|)'], digits=3)
    
    if(p_value_intercept > p_value_threshold){
      lm_fit <- rlang::eval_tidy(
        rlang::quo(
          lm(formula=UQ(y)~-1+UQ(x), data=newdf, weights=wts)
        )
      )
      lm_sum <- summary(lm_fit)
    }
    
    # plot settings 
    y_max <- max(df[[rlang::expr_text(y)]], na.rm=T)
    y_max_pos <- y_max*max_position
    y_pval <- y_max*min_p_position
    y_min_tab <- y_max*min_tab_position
    
    x_max <- max(df[[rlang::expr_text(x)]], na.rm=T)
    x_diff <- (x_max - x_min) / 4
    x_min_pos <- x_min + x_diff
    x_max_pos <- x_max - x_diff
    
    # plots without parameters
    if(!plot.par){
      # 1. without intercept
      if(p_value_intercept>p_value_threshold){
        p <- rlang::quo(
          ggplot(newdf, aes(x=!!x, y=!!y)) + 
            geom_point() + 
            geom_smooth(aes(weight=wts), method="lm", formula=y~-1+x, 
                        se=T, level=ci.level) + 
            coord_cartesian(xlim = c(!!x_min, !!x_max), ylim = c(0, !!y_max_pos)) + 
            labs(title="Weighted Linear Regression Model") + 
            annotate(geom="text", x=!!x_min_pos, y=!!y_pval, 
                     label=paste0("p value for intercept = ", !!p_value_intercept))
        )
      } 
      # 2. with intercept
      if(p_value_intercept<=p_value_threshold){
        p <- rlang::quo(
          ggplot(newdf, aes(x=!!x, y=!!y)) + 
            geom_point() + 
            geom_smooth(aes(weight=wts), method="lm", formula=y~x, 
                        se=T, level=ci.level) + 
            coord_cartesian(xlim = c(!!x_min, !!x_max), ylim = c(0, !!y_max_pos)) + 
            labs(title="Weighted Linear Regression Model")
        )
      }
    }
    
    # plots with parameters
    if(plot.par){
      # Extract the data from the summary to a data.frame for plotting
      # 1. lm parameter data 
      lm_data <- as.data.frame(lm_sum$coefficients)
      lm_data <- format(lm_data, digits=signif)
      lm_data <- tibble::rownames_to_column(lm_data)
      names(lm_data)[1] <- "Parameter"
      
      # Rename parameters to intercept and slope
      if(any(str_detect(lm_data$Parameter, "\\(Intercept\\)"))){
        lm_data$Parameter <- 
          str_replace(lm_data$Parameter, "\\(Intercept\\)", "Intercept")
      }
      lm_data$Parameter <- 
        str_replace(lm_data$Parameter, rlang::expr_text(x), "Slope")
      
      # 1. without intercept
      if(p_value_intercept>p_value_threshold){
        p <- rlang::quo(
          ggplot(newdf, aes(x=!!x, y=!!y)) + 
            # add lm parameters (first layer to make sure not overplotting outliers)
            annotation_custom(gridExtra::tableGrob(lm_data, rows=NULL),
                              xmin=!!x_min_pos, xmax=!!x_max_pos,
                              ymin=!!y_min_tab, ymax=!!y_max_pos) + 
            geom_point() + 
            geom_smooth(aes(weight=wts), method="lm", formula=y~-1+x, 
                        se=T, level=ci.level) + 
            coord_cartesian(xlim = c(!!x_min, !!x_max), ylim = c(0, !!y_max_pos)) + 
            labs(title="Weighted Linear Regression Model") + 
            annotate(geom="text", x=!!x_min_pos, y=!!y_pval, 
                     label= paste0("p value for intercept = ", !!p_value_intercept))
        )
      } 
      # 2. with intercept
      if(p_value_intercept<=p_value_threshold){
        p <- rlang::quo(
          ggplot(newdf, aes(x=!!x, y=!!y)) + 
            # add lm parameters (first layer to make sure not overplotting outliers)
            annotation_custom(gridExtra::tableGrob(lm_data, rows=NULL),
                              xmin=!!x_min_pos, xmax=!!x_max_pos,
                              ymin=!!y_min_tab, ymax=!!y_max_pos) + 
            geom_point() + 
            geom_smooth(aes(weight=wts), method="lm", formula=y~x, 
                        se=T, level=ci.level) + 
            coord_cartesian(xlim = c(!!x_min, !!x_max), ylim = c(0, !!y_max_pos)) + 
            labs(title="Weighted Linear Regression Model")
        )
      }
    }
    return(list(plot = rlang::eval_tidy(p), summary=summary(lm_fit), fit=lm_fit))
  }
