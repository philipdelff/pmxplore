# 
# lm_model <- lm(data=mytestdata, auc~dose)
# power_model <- lm(data=mytestdata, log(auc)~log(dose))
# 
# test <-
#   dose_proportionality_anova(mytestdata, x=dose, y=auc,  plot.par=F)
# test[['plot']] + theme_bw()
# 
# test <-
#   dose_proportionality_anova(mytestdata, x=dose, y=auc,  plot.par=T)
# test[['plot']] + theme_bw()
# 
# test <-
#   dose_proportionality_linear(mytestdata, x=dose, y=auc,  plot.par=T, x_min=100)
# test[['plot']] + theme_bw()
# 
# test <-
#   dose_proportionality_power(mytestdata, x=dose, y=auc, plot.par = T)
# test[['plot']] + theme_bw()