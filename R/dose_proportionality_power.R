#' @title Dose proportionality test (power model)
#' @description log(y)~log(y) lm fit to test for dose proportionality
#' @param df data frame
#' @param x x variable (e.g. dose) passed in untransformed
#' @param y y parameter (e.g. auc) passed in untransformed
#' @param ci.level confidence level for lm fit in plot, Default: 0.95
#' @param plot.par add table with fit parameters, Default: F
#' @param max_position upper y limit passed to coord_carteesian (relative max(y)), Default: 1.1
#' @param min_tab_position lower y-value for parameter table (relative max(y)), Default: 1.05
#' @param min_text_position position of text string (relative max(y)), Default: 1
#' @param signif number of significant numbers in parameter table, Default: 3
#' @return list with plot, power lm summary and power lm fit
#' @rdname dose_proportionality_power
#' @export 
#' @importFrom gridExtra tableGrob
#' @importFrom rlang enexpr expr_text sym eval_tidy quo
#' @importFrom tibble rownames_to_column
#' @import magrittr

dose_proportionality_power <- 
  function(df, x, y, ci.level=0.95, 
           plot.par=F, 
           max_position = 1.2,
           min_tab_position = 1.05,
           min_text_position = 0.90, 
           signif=3){
    
    x <- rlang::enexpr(x)
    y <- rlang::enexpr(y)
    
    log_x_name <- paste0("log_", rlang::expr_text(x))
    log_y_name <- paste0("log_", rlang::expr_text(y))
    
    log_x <- rlang::sym(log_x_name)
    log_y <- rlang::sym(log_y_name)
    
    # 1. Add log of x and y
    newdf <- rlang::eval_tidy(
      rlang::quo(df %>% 
                   mutate(!!log_x_name := log(!!x), 
                          !!log_y_name := log(!!y))
      )
    )
    
    # Fit power model lm 
    lm_power_fit <- rlang::eval_tidy(
      rlang::quo(
        lm(formula=!!log_y~!!log_x, data=newdf)
      )
    )
    lm_power_sum <- summary(lm_power_fit)
    
    # find max and min values of x and y, log(x) and log(y)
    x_max_log <- max(newdf[[log_x_name]], na.rm=T)
    x_min_log <- min(newdf[[log_x_name]], na.rm=T)
    
    y_max <- max(newdf[[rlang::expr_text(y)]], na.rm=T)
    y_min <- min(newdf[[rlang::expr_text(y)]], na.rm=T)
    
    x_max <- max(newdf[[rlang::expr_text(x)]], na.rm=T)
    x_min <- min(newdf[[rlang::expr_text(x)]], na.rm=T)
    
    # predict values and back-transform
    x_pred    <- data.frame(x=seq(x_min_log, x_max_log, length=100)) # improve. by as a par?
    names(x_pred) <- log_x_name
    predicted <- predict.lm(lm_power_fit, x_pred,
                            se.fit=T, 
                            interval=c("confidence"), 
                            level=ci.level)
    # put in data frame and back-tranform
    pred_data <- data.frame(x = exp(x_pred[,1]), 
                            y = exp(predicted$fit[,'fit']), 
                            lwr = exp(predicted$fit[,'lwr']), 
                            upr = exp(predicted$fit[,'upr']))

    # calculate critical region
    x_ratio <- x_max / x_min
    critial_region <- format(
      c( 1 + ( log(0.80)/log(x_ratio) ), 
         1 + ( log(1.25)/log(x_ratio) ) ),
      digits=signif)
    critial_region_string <- 
      paste0("Critical Region = [",
             critial_region[1],",",
             critial_region[2],"]")
    
    # Extract the data from the summary to a data.frame for plotting
    # 1. lm parameter data 
    lm_power_data <- as.data.frame(lm_power_sum$coefficients)
    lm_power_data <- format(lm_power_data, digits=signif)
    lm_power_data <- tibble::rownames_to_column(lm_power_data)
    names(lm_power_data)[1] <- "Parameter"
    
    # Rename parameters to intercept and slope
    lm_power_data$Parameter <- 
      str_replace(lm_power_data$Parameter, "\\(Intercept\\)", "Intercept")
    lm_power_data$Parameter <-
      str_replace(lm_power_data$Parameter, log_x_name, "Power")
    
    power_est <- as.numeric(lm_power_data$Estimate[lm_power_data$Parameter=="Power"])
    power_sd <- as.numeric(lm_power_data$`Std. Error`[lm_power_data$Parameter=="Power"])
    
    lm_power_string <- paste0("Power [95% CI] = ",
                              power_est," [", 
                              signif(power_est - 1.96*power_sd, digits=signif), "-",
                              signif(power_est + 1.96*power_sd, digits=signif), "]")
    
    # plot settings
    y_max_pos <- y_max*max_position
    y_text <- y_max*min_text_position
    y_min_tab <- y_max*min_tab_position

    x_diff <- (x_max - x_min) / 4
    x_min_pos <- x_min + x_diff
    x_max_pos <- x_max - x_diff

    # plots without parameters
    if(!plot.par){
      p <- rlang::quo(
        ggplot(newdf, aes(x=!!x, y=!!y)) + 
          geom_point() + 
          # add predicted line (defaults from geom_smooth defaults)
          geom_line(data = pred_data, aes(x=x, y=y), inherit.aes=F, 
                    color="#3366FF", size=1) +
          # add se (defaults from geom_smooth defaults)
          geom_ribbon(data=pred_data, aes(x=x, ymin=lwr,ymax=upr), 
                      fill="grey60", alpha="0.4", inherit.aes=F) +
          coord_cartesian(ylim = c(y_min, !!y_max_pos)) + 
          labs(title="Power model") +
          # give CI of power estimate and critical region
          annotate(geom="text", x=!!x_min_pos, y=!!y_text, 
                   label=paste(lm_power_string, critial_region_string, sep="\n"))
      )
    }
    
    # plots with parameters
    if(plot.par){
      p <- rlang::quo(
        ggplot(newdf, aes(x=!!x, y=!!y)) + 
          # add lm parameters (first layer to make sure not overplotting outliers)
          annotation_custom(gridExtra::tableGrob(lm_power_data, rows=NULL),
                            xmin=!!x_min_pos, xmax=!!x_max_pos,
                            ymin=!!y_min_tab, ymax=!!y_max_pos) + 
          geom_point() + 
          # add predicted line (defaults from geom_smooth defaults)
          geom_line(data = pred_data, aes(x=x, y=y), inherit.aes=F, 
                    color="#3366FF", size=1) +
          # add se (defaults from geom_smooth defaults)
          geom_ribbon(data=pred_data, aes(x=x, ymin=lwr,ymax=upr), 
                      fill="grey60", alpha="0.4", inherit.aes=F) +
          coord_cartesian(ylim = c(y_min, !!y_max_pos)) + 
          labs(title="Power model") +
          # give CI of power estimate and critical region
          annotate(geom="text", x=!!x_min_pos, y=!!y_text, 
                   label=critial_region_string)
      )
    }
    return(list(plot = rlang::eval_tidy(p), 
                summary=summary(lm_power_fit), 
                fit=lm_power_fit))
}
