#' Modified Theoph 
#' 
#' Theoph dataset with nonmem specific columns for testing of functions
#' 
#' @format A data.frame with 8 columns and 144 rows
#' \describe{
#' \item{Subject}{an ordered factor with levels 1, ..., 12 identifying the 
#' subject on whom the observation was made. The ordering is by increasing 
#' maximum concentration of theophylline observed.}
#' \item {Wt}{weight of the subject (kg).}
#' \item {Dose}{dose of theophylline administered orally to the subject (mg/kg).}
#' \item {Time}{time since drug administration when the sample was drawn (hr).}
#' \item {conc}{theophylline concentration in the sample (mg/L).}
#' \item {AMT}{amount}
#' \item {EVID}{event identifier (0=dependent variable, 1=dosing)}
#' \item {MDV}{missing dependent variable (0=No, 1=Yes)}
#' }
#' @source Theoph with additions
#' @seealso {datasets}{Theoph}
"theophNM"