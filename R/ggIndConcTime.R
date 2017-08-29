#' @title create individual concentration time plot
#' @param df dataframe
#' @param obs name of observation column , Default: DV
#' @param pred name of prediction column, Default: PRED
#' @param ipred name of individual prediction column, Default: IPRED
#' @param x name of x variable, Default: TAPD
#' @param id name of ID column, Default: ID
#' @param occ name of OCC column, Default: OCC
#' @param facetRows number of rows to factet on, Default: 3
#' @rdname ggIndConcTime
#' @export 
#' @importFrom rlang enexpr eval_tidy quo
gg_ind_conc_time <- function(df, obs=DV, pred=PRED, ipred=IPRED,
                        x=TAPD, id=ID, occ=OCC,
                        facetRows=3, ...){
  
  obs   <- rlang::enexpr(obs)
  pred  <- rlang::enexpr(pred)
  ipred <- rlang::enexpr(ipred)
  x     <- rlang::enexpr(x)
  id    <- rlang::enexpr(id)
  occ   <- rlang::enexpr(occ)
  
  rich <- rlang::eval_tidy(
    rlang::quo(df %>%  
                 subset(!is.na(!!occ)) %>% 
                 mutate(ID.OCC = factor(paste(!!id, !!occ, sep=".")))))
  sparse <- rlang::eval_tidy(
    rlang::quo(df %>% 
                 subset(is.na(!!occ)) %>% 
                 mutate(ID.OCC = factor(paste(!!id, !!occ, sep=".")))))
  
  p <- rlang::quo(
    ggplot(data=rich) + 
      # Start with adding population pred (rich and sparse)
      geom_line(data=rich, aes(y=!!pred,  x=!!x, group=ID.OCC),
                colour="darkgray", linetype="dashed", inherit.aes=F) +
      geom_point(data=sparse, aes(y=!!pred, x=!!x), shape=3,
                 colour="gray", inherit.aes=F) +
      # Rich data: observed
      geom_point(aes(y=!!obs, x=!!x, colour=!!occ, group=ID.OCC), shape=1) +
      # Rich data: individual prediction
      geom_line(data=rich, aes(y=!!ipred, x=!!x, colour=!!occ, group=ID.OCC)) +
      
      # Sparse data: observed 
      geom_point(data=sparse, aes(y=!!obs, x=!!x), shape=1, inherit.aes=F) + 
      # Rich data: individual prediction (no lines)
      geom_point(data=sparse, aes(y=!!ipred, x=!!x), shape=3, inherit.aes=F) +
      # settings
      facet_wrap(~!!id, nrow=facetRows, labeller="label_both", scales="free_y")
  )
  return(eval_tidy(p))
}
