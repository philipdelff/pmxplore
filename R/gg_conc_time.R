#' @title Concentration vs time plot
#' @description Conc time plots with stratification
#' @param df data frame
#' @param y y variable, Default: DV
#' @param x x variable, Default: TAFD
#' @param color grouping variable passed to aes (=color), Default: ID
#' @param occ name of occasion column. If not NULL used as grouping variable (see details), Default: NULL
#' @param blq name of blq flag column, Default: NULL
#' @return ggplot object
#' @details If occ is provided, color and occ are combined using paste to create a new grouping variable for each occasion. 
#' Lines only connect data from each unique color+occ group. Useful for time after dose graphics.
#' @rdname gg_conc_time
#' @export 
#' @importFrom rlang enexpr quo expr_text eval_tidy
#' @importFrom dplyr filter mutate
#' @import ggplot2

gg_conc_time <- 
  function(df, y=DV, x=TAFD, color=ID, occ=NULL, blq=NULL){
    
    y     <- rlang::enexpr(y)
    x     <- rlang::enexpr(x)
    color <- rlang::enexpr(color)
    
    occ   <- rlang::enexpr(occ)
    blq   <- rlang::enexpr(blq)
    
    # Checks of data: present in dataset and not all NAs?

    # Color is required:
    if(is.null(color) ){
      stop("Color variable is required")
    }
    if(! rlang::expr_text(color) %in% names(df)){
      stop(paste0(rlang::expr_text(color), " not found in dataset."))
    }
    if(all(is.na(df[[rlang::expr_text(color)]]))){
      stop(paste0("All entries of ", rlang:expr_text(color), " are NA."))
    }

    # Occ and blq are optional:
    if(!is.null(occ)){
      if(! rlang::expr_text(occ) %in% names(df)){
        stop(paste0(rlang::expr_text(occ), " not present in dataset."))
      }
      
      if(all(is.na(df[[rlang::expr_text(occ)]]))){
        stop(paste0("All entries of ", rlang::expr_text(occ), " are NA."))
      }
    }
    if(!is.null(blq)){
      if(! rlang::expr_text(blq) %in% names(df)){
        stop(paste0(rlang::expr_text(blq), " not present in dataset."))
      }
      
      if(all(is.na(df[[rlang::expr_text(blq)]]))){
        stop(paste0("All entries of ", rlang::expr_text(blq), " are NA."))
      }
    }
    
    # Dataset for geom_line (i.e., does not connect sparse occasions)
    if(!is.null(occ)){
      rich <- rlang::quo(df %>%  
                           dplyr::filter(!is.na(!!occ)) %>% 
                           dplyr::mutate(grp = factor(paste(!!color, !!occ, sep= "."))))
      rich <- rlang::eval_tidy(rich)
    }

    # different point layers if blq flag or not
    if(is.null(blq)){
      point_layer <- rlang::quo(geom_point()) 
    }else{
      point_layer <- rlang::quo(geom_point(aes(shape=!!blq)))
    }
    
    # Generate plot (with or without the grp variable for geom_line)
    if(is.null(occ)){
      p <- rlang::quo(
        ggplot(data=df, aes(x= !!x, y= !!y , colour= !!color)) + 
          rlang::eval_tidy(point_layer) + 
          geom_line()
      )
    } else {
      p <- rlang::quo(
        ggplot(data=df, aes(x= !!x, y= !!y , colour= !!color)) + 
          rlang::eval_tidy(point_layer) + 
          geom_line(data=rich, 
                    aes(x= !!x, y= !!y, 
                        colour= !!color, group= grp), inherit.aes = F) 
      )
    }
    return(rlang::eval_tidy(p))
  }