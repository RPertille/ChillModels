% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdh_a.R
\name{gdha_model}
\alias{gdha_model}
\title{GDH Model - Anderson - for heat accumulation}
\usage{
gdha_model(x, total = TRUE)
}
\arguments{
\item{x}{Vector containing temperature values (Celsius-degree).}

\item{total}{TRUE Shows the total value of accumulation, FALSE shows the value of GDH for each temperature (TRUE is default).}
}
\value{
The function returns values the GDH for each temperature of vector.
}
\description{
Quantifies the Growing Degree Hours at between the base and optimum temperatures, by Anderson et al. (1986).
}
\details{
The GDH Model is based on base and optimum temperature. The base temperature is 4°C. The optimum and critical temperature are 25°C and 36°C, respectively.
}
\examples{

x <- rnorm(500,8,3)
gdha_model(x)
gdha_model(x, FALSE)

}
\references{
Anderson, J. L. et al. 1986. Validation of chill unit and flower bud phenology models for "Montmorency" sour cherry. Acta Horticulturae - Modelling in Fruit Research.
}
