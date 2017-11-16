#' @title Individual concentration time plot
#' @param df dataframe
#' @param obs observed data column , Default: DV
#' @param pred population prediction column, Default: PRED
#' @param ipred individual prediction column, Default: IPRED
#' @param x x variable, Default: TAPD
#' @param id id variable, Default: ID
#' @param occ occasion column. If not NULL used as grouping variable (see details), Default: NULL
#' @param facet_rows rows used in facet_wrap, Default: 3
#' @param facet_scales scales used facet_wrap, Default: "free"
#' @details If occ is provided, id and occ are combined using paste to create a new grouping variable for each occasion. 
#' Lines only connect data from each unique color+occ group. Useful for time after dose graphics.
#' Population predictions: dashed lines and triangles,
#' Individal predictions: lines and crosses, 
#' Observed data: open circles.
#' @rdname gg_conc_time_ind
#' @export 
#' @importFrom rlang enexpr eval_tidy expr_text quo
#' @importFrom dplyr filter mutate
#' @import ggplot2

gg_conc_time_ind <- function(df, obs=DV, pred=PRED, ipred=IPRED, x=TAPD,
                             id=ID, occ=NULL, 
                             facet_rows=3, facet_scales = "free", ...){
  
  obs   <- rlang::enexpr(obs)
  pred  <- rlang::enexpr(pred)
  ipred <- rlang::enexpr(ipred)
  x     <- rlang::enexpr(x)
  
  id    <- rlang::enexpr(id)
  occ   <- rlang::enexpr(occ)
  
  ## Checks of data: present in dataset and not all NAs? ##
  
  # id is required:
  if(is.null(id) ){
    stop("id variable is required")
  }
  if(! rlang::expr_text(id) %in% names(df)){
    stop(paste0(rlang::expr_text(id), " not found in dataset."))
  }
  if(all(is.na(df[[rlang::expr_text(id)]]))){
    stop(paste0("All entries of ", rlang:expr_text(id), " are NA."))
  }
  
  # Occ is optional:
  if(!is.null(occ)){
    if(! rlang::expr_text(occ) %in% names(df)){
      stop(paste0(rlang::expr_text(occ), " not present in dataset."))
    }
    
    if(all(is.na(df[[rlang::expr_text(occ)]]))){
      stop(paste0("All entries of ", rlang::expr_text(occ), " are NA."))
    }
    
    # warning if occ not a factor
    if(!is.factor(df[[rlang::expr_text(occ)]])){
      warning(paste(rlang::expr_text(occ), "is not a factor. Unexpected results may occur"))
    }
  }
  
  ### Generate dataset for geom_line (does not connect sparse occasions)  ### 
  if(!is.null(occ)){
    rich <- rlang::quo(df %>%  
                         dplyr::filter(!is.na(!!occ)) %>% 
                         dplyr::mutate(grp = factor(paste(!!id, !!occ, sep= "."))))
    rich <- rlang::eval_tidy(rich)
    
    sparse <- rlang::quo(df %>% 
                           dplyr::filter(is.na(!!occ)) %>%
                           dplyr::mutate(grp = factor(paste(!!id, !!occ, sep="."))))
    
    sparse <- rlang::eval_tidy(sparse)
  }
  
  # Generate plot (without or with the grp variable for geom_line)
  if(is.null(occ)){
    
    p <- rlang::quo(
      ggplot(data=df) + 
        # Pred
        geom_line(aes(y=!!pred,  x=!!x, group=!!id),
                  colour="darkgray", linetype="dashed", inherit.aes=F) +
        # Observed
        geom_point(aes(y=!!obs, x=!!x, group=!!id), shape=1) +
        # Individual prediction
        geom_line(aes(y=!!ipred, x=!!x, group=!!id)) +
        
        # settings
        facet_wrap(~!!id, nrow=facet_rows, scales=facet_scales, labeller="label_both")
    )
    
  } else {
    
    p <- rlang::quo(
      ggplot(data=rich) + 
        # Add population pred (rich and sparse)
        geom_line(data=rich, aes(y=!!pred,  x=!!x, group=grp),
                  colour="darkgray", linetype="dashed", inherit.aes=F) +
        geom_point(data=sparse, aes(y=!!pred, x=!!x), shape=2,
                   colour="gray", inherit.aes=F) +
        
        # Rich data: observed
        geom_point(aes(y=!!obs, x=!!x, colour=!!occ, group=grp), shape=1) +
        # Rich data: individual prediction
        geom_line(data=rich, aes(y=!!ipred, x=!!x, colour=!!occ, group=grp)) +
        
        # Sparse data: observed 
        geom_point(data=sparse, aes(y=!!obs, x=!!x), shape=1, inherit.aes=F) + 
        # Sparse data: individual prediction (no lines)
        geom_point(data=sparse, aes(y=!!ipred, x=!!x), shape=3, inherit.aes=F) +
        
        # settings
        facet_wrap(~!!id, nrow=facet_rows, scales=facet_scales, labeller="label_both")
    )
  } 
  
  return(eval_tidy(p))
}