###################################################
# ggConcTime.R
# 
# Author: Helena Edlund
# Created on: 2017-08-10
# Modified on: 
# Purpose: Conc time plots with stratification
# Dependencies: ggplot2, rlang
###################################################

ggConcTime <- 
  function(df, y=DV, x=TAFD, color=ID, grp=ID.OCC, 
           occ=OCC, blq=BLQ, lloq=1){
    y     <- rlang::enexpr(y)
    x     <- rlang::enexpr(x)
    color <- rlang::enexpr(color)   # color should be passed in as factor
    grp   <- rlang::enexpr(grp)
    
    occ   <- rlang::enexpr(occ)
    blq   <- rlang::enexpr(blq)
    lloq <- rlang::enexpr(lloq)
    
    # Dataset for lines (i.e., do not connect sparse occasions)
    rich <- rlang::quo(
      df %>% filter(!is.na(!!occ))) 
    rich <- eval_tidy(rich)
    
    p <- rlang::quo(
      ggplot(data=df, aes(x= !!x, y= !!y, colour= !!color)) +
        geom_point(aes(shape=!!blq)) +
        
        geom_line(data=rich, aes(x= !!x, y= !!y,
                                 colour= !!color, group= !!grp), inherit.aes = F) +
        
        geom_hline(aes(yintercept = !!lloq), linetype="dashed", col="blue") 
    )
    
    return(eval_tidy(p))
    
  }