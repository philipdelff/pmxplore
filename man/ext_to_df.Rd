% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ext_to_df.R
\name{ext_to_df}
\alias{ext_to_df}
\title{Generate parameter table suitable for reports}
\usage{
ext_to_df(modelFile, digits = 4)
}
\arguments{
\item{modelFile}{name of NONMEM output files *without* the .ext extension (including full or relative path to file)}

\item{digits}{number of significant digits, Default: 4}
}
\value{
data frame
}
\description{
Loads NONMEM output files (lst, ext, and if covariance setp was run, the cov file) using 
nonmem2R::sumoR and compiles a data.frame with parameter estiamtes, RSE and Shrinkage suitable for report. 
The resulting data.frame suitable as input to kableExtra.
}
\details{
DETAILS
}
\seealso{
\code{\link[nonmem2R]{sumoR}}
}
