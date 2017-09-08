
dose_proportionality_linear <- 
  function(df, x, y, plot.par=F, max_position = 1.25,
           min_position = 0.85, ci.level=0.95){
    
    x <- rlang::enexpr(x)
    y <- rlang::enexpr(y)
    
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
    
    if(p_value_intercept > 0.05){
      lm_fit <- rlang::eval_tidy(
        rlang::quo(
          lm(formula=UQ(y)~-1+UQ(x), data=newdf, weights=wts)
        )
      )
      lm_sum <- summary(lm_fit)
    }
    
    # plot settings 
    y_max <- max(df[[rlang::expr_text(y)]], na.rm=T)*max_position
    y_min_pos <- max(df[[rlang::expr_text(y)]], na.rm=T)* min_position
    
    x_max <- max(df[[rlang::expr_text(x)]], na.rm=T)
    x_min_tab <- max(df[[rlang::expr_text(x)]], na.rm=T)/4
    
    # plot without intercept
    if(!plot.par & p_value_intercept>0.05){
      p <- rlang::quo(
        ggplot(newdf, aes(x=!!x, y=!!y)) + 
          geom_point() + 
          geom_smooth(aes(weight=wts), method="lm", formula=y~-1+x, 
                      se=T, level=ci.level) + 
          coord_cartesian(xlim = c(0, !!x_max), ylim = c(0, !!y_max)) + 
          annotate(geom="text", xmin=!!x_min_tab, xmax=!!x_max,  ymin=!!y_min_pos, ymax=!!y_max, 
                   label= paste("Weighted Linear Regression Model/n", 
                                "p value for intercept = ", !!p_value_intercept))
        
  # update:  annotate with geom_text should only have x and y
        
      )
    } 
    # plot with intercept
    if(!plot.par & p_value_intercept<=0.05){
      p <- rlang::quo(
        ggplot(newdf, aes(x=!!x, y=!!y)) + 
          geom_point() + 
          geom_smooth(aes(weight=wts), method="lm", formula=y~+x, 
                      se=T, level=ci.level) + 
          coord_cartesian(xlim = c(0, !!x_max), ylim = c(0, !!y_max))+ 
          annotate(geom="text", xmin=!!x_min_tab, xmax=!!x_max,  ymin=!!y_min_pos, ymax=!!y_max, 
                   label= paste("Weighted Linear Regression Model/n", 
                                "p value for intercept = ", !!p_value_intercept))
      )
    }
    
    #     
    # # else: 
    #     text(c(bb[1],bb[1]),c(b*0.95,b*0.90),c("Intercept","Slope"),cex=cex*0.68)
    #     for (i in 2:(length(bb))){
    #       text(c(bb[i],bb[i]),c(b*0.95,b*0.90),c(round(summary(lm.fit)$coe[1,],3)[i-1],round(summary(lm.fit)$coe[2,],3)[i-1]),cex=cex*0.68)
    #     }
    #   }
    #   
    #   
    #   if(plot.par){
    #     rowname<-c("Parameter",names(summary(lm.fit)$coe[1,]))
    #     bb<-seq(1,max(x),max(x)/5)
    #     for (i in 1:length(bb)){text(bb[i],b,rowname[i],cex=cex*0.68)}  
    #     abline(h=b*0.85)
    #     n<-nrow(summary(lm.fit)$coe)
    #     if (n==1) {
    #       text(bb[1],b*0.90,"Slope",cex=cex*0.68)
    #       for (i in 3:(length(bb))) {text(bb[i],b*0.90,round(summary(lm.fit)$coe,2)[i-1],cex=cex*0.68)}}
    #   }
    return(list(plot = rlang::eval_tidy(p), summary=summary(lm_fit)))
  }


