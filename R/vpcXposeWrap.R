###################################################
# vpcXposeWrap.R
# 
# Author: Helena Edlund
# Created on: 2017-03-23
# Modified on: 2018-08-02
# Purpose: customized vpc call
# Dependencies: xpose4
###################################################

myVPC <- function(vpcresultsfile, vpctabfile, type="p", 
                  xlab = "Time after dose (h)",
                  ylab= "Concentrations (ng/mL)") {
  p <- xpose.VPC(vpc.info = vpcresultsfile,
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
