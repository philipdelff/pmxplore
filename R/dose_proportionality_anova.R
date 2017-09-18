#' @title Dose proportionality test using anova
#' @description Anova fit to test for dose proportionality
#' @param df PARAM_DESCRIPTION
#' @param x x variable (e.g. dose) 
#' @param y y variable (e.g. auc)
#' @param plot.par print anova parameters as table, Default: F
#' @param max_position multiplier to max(y) for positioning text annotation, Default: 1.25
#' @param min_position multiplier to max(y) for positioning text annotation, Default: 0.85
#' @param signif PARAM_DESCRIPTION, Default: 3
#' @return a list of ggplot object, summary of anova fit and the fit object
#' @details Table with parameters extend between min_position*max(y) and max_position*max(y) 
#' @rdname dose_proportionality_anova
#' @export 
#' @importFrom gridExtra tableGrob
#' @importFrom rlang enexpr expr_text quo eval_tidy

dose_proportionality_anova <- 
  function(df, x, y, plot.par=F, max_position = 1.25, min_position = 0.85, 
           signif=3){
    x <- rlang::enexpr(x)
    y <- rlang::enexpr(y)

    # Check that x and y is in dataset
    if(!rlang::expr_text(x) %in% names(df)){
      stop(paste0(rlang::expr_text(x), " not found in dataset"))
    }
    if(!rlang::expr_text(y) %in% names(df)){
      stop(paste0(rlang::expr_text(y), " not found in dataset"))
    }
    
    # extract data   
    x_col <- df[[rlang::expr_text(x)]]
    y_col <- df[[rlang::expr_text(y)]]
    
    # Warning for not factor
    if(!is.factor(x_col)){
      message(paste(rlang::expr_text(x), "must be categorical. The structure of", 
                    rlang::expr_text(x), "was therefore set to factor."))
      x_col <- as.factor(x_col)
      df[[rlang::expr_text(x)]] <- as.factor(df[[rlang::expr_text(x)]])
    }
    
    # anova fit
    aov_fit <- aov(y_col~x_col)
    aov_sum <- summary(aov_fit)
    
    if(!plot.par){
      # Extract p value
      p_value <- round(unlist(aov_sum)["Pr(>F)1"], digits=3)
      if (p_value==0){p_value <- "0.000"}
      x_pos <- (length(levels(x_col))+1)/2 # center of x
      
      # force larger plot area to fit printing of parameters
      y_max <- max(y_col, na.rm=T)*max_position    
      
      p <- rlang::quo(
        ggplot(df, aes(x=!!x, y=!!y)) + 
          geom_boxplot() + 
          coord_cartesian(ylim = c(0,!!y_max)) + 
          geom_hline(aes(yintercept=median(!!y, na.rm=T)), linetype="dashed") + 
          annotate(geom="text", x=!!x_pos, y=!!y_max,
                   label=paste0("p value = ", !!p_value," (ANOVA)"))
      )
    }
    
    if(plot.par){
      # Extract the data from the summary to a data.frame for plotting
      # 1. anova parameter data 
      aov_pars <- unlist(lapply(aov_sum, names))
      n_cols <- length(aov_pars)  # is this ever different from 5?
      # 2. set up empty data frame
      aov_data_m <- matrix(nrow=2, ncol=n_cols)    
      aov_data <- as.data.frame(aov_data_m)
      names(aov_data) <- aov_pars
      # 3. add for which parameter
      aov_data$Parameter <- c(rlang::expr_text(x), "Residuals") 
      # 4. extract the data
      for(i in aov_pars){
        aov_data[, i] <- signif(aov_sum[[1]][[i]], digits=signif)
      }
      # reorder with parameter column first
      aov_data <- aov_data[, c("Parameter", 
                               names(aov_data)[names(aov_data) !="Parameter"])]
      
      # force larger plot area to fit printing of parameters
      y_max <- max(y_col, na.rm=T)*max_position
      y_min_pos <- y_max*min_position
      
      # parameter table position
      x_min_tab <- 0.5
      x_max_tab <- length(levels(x_col))+0.5
      
      # plot
      p <- rlang::quo(
        ggplot(data=df, aes(x=!!x, y=!!y)) + 
          # add anova parameters (first layer to make sure not overplotting outliers)
          annotation_custom(gridExtra::tableGrob(aov_data, rows=NULL),
                            xmin=!!x_min_tab, xmax=!!x_max_tab,
                            ymin=!!y_min_pos, ymax=!!y_max) + 
          geom_boxplot() + 
          coord_cartesian(ylim = c(0,!!y_max)) + 
          geom_hline(aes(yintercept = median(!!y)), linetype="dashed") # median
      )
    }
    return(list(plot = rlang::eval_tidy(p), summary = aov_sum, fit_object=aov_fit))
  }
# should we be using a continuous boxplot instead? 