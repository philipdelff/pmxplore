#' @title FUNCTION_TITLE
#' @description Conc time plots with stratification
#' @param df data frame
#' @param y y variable, Default: DV
#' @param x x variable, Default: TAFD
#' @param color color variable passed to aes, should be factor, Default: ID
#' @param grp grouping variable passed to aes, Default: ID.OCC
#' @param occ col name for occasion column, Default: OCC
#' @param blq col name for blq flag column, Default: BLQ
#' @param lloq lower limit of quantification value, Default: 1
#' @param lloq_col color of lloq line, Default: "blue"
#' @param lloq_type linetyp of lloq line, Default: "dashed"
#' @return ggplot object
#' @details DETAILS
#' @rdname gg_conc_time
#' @export 
#' @importFrom rlang enexpr quo
#' @importFrom dplyr filter 
#' @import ggplot2

gg_conc_time <- 
  function(df, y=DV, x=TAFD, color=ID, grp=ID.OCC, 
           occ=OCC, blq=BLQ,
           lloq=1, lloq_col="blue", lloq_type="dashed"){
    y     <- rlang::enexpr(y)
    x     <- rlang::enexpr(x)
    color <- rlang::enexpr(color)   
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
        
        geom_hline(aes(yintercept = !!lloq), linetype=!!lloq_type, col=!!lloq_col) 
    )
    return(eval_tidy(p))
  }
