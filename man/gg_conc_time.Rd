% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_conc_time.R
\name{gg_conc_time}
\alias{gg_conc_time}
\title{Concentration vs time plot}
\usage{
gg_conc_time(df, y = DV, x = TAFD, color = ID, occ = NULL, blq = NULL)
}
\arguments{
\item{df}{data frame}

\item{y}{y variable, Default: DV}

\item{x}{x variable, Default: TAFD}

\item{color}{grouping variable passed to aes (=color), Default: ID}

\item{occ}{name of occasion column. If not NULL used as grouping variable (see details), Default: NULL}

\item{blq}{name of blq flag column, Default: NULL}
}
\value{
ggplot object
}
\description{
Conc time plots with stratification
}
\details{
If occ is provided, color and occ are combined using paste to create a new grouping variable for each occasion. 
Lines only connect data from each unique color+occ group. Useful for time after dose graphics.
}
