#' @title customized xpose vpc
#' @description wrapper for xpose vpc
#' @param vpcresultsfile psn vpc output file
#' @param vpctabfile psn vpc output file ("vpctabXXX")
#' @param type with our without data, Default: 'p'
#' @param xlab string with x-axis label, Default: 'Time after dose (h)'
#' @param ylab string with y-axis label, Default: 'Concentrations (ng/mL)'
#' @return lattice plot
#' @seealso 
#'  \code{\link[xpose4]{xpose.VPC}}
#' @rdname vpc_xpose_wrap
#' @export 
#' @importFrom xpose4 xpose.VPC

vpc_xpose_wrap <- function(vpcresultsfile = "vpc_results.csv",
                           vpctabfile,
                           type="p", 
                           xlab = "Time after dose (h)",
                           ylab= "Concentrations (ng/mL)") {
  p <- xpose4::xpose.VPC(vpc.info = vpcresultsfile,
                         vpctab = vpctabfile,
                         main = NULL,
                         type = type,
                         xlb = xlab,
                         ylb = ylab,
                         col="darkgray",
                         PI.real.up.col="black",
                         PI.real.down.col="black",
                         PI.real.med.col="black", 
                         PI.real.up.lty="dotted",
                         PI.real.down.lty="dotted",
                         PI.ci.up.arcol = "#7570b3",
                         PI.ci.down.arcol = "#7570b3",
                         PI.ci.med.arcol = "#1b9e77")
  return(p)
}
