\name{LL}
\alias{LL}
\docType{data}
\title{
Practice dataset for list length analysis
}
\description{
Data obtained from bird lists in the standard format (see \code{A3Model} or {A5Model})
}
\usage{data(LL)}
%% \format{
%%   A data frame with records (lists) for 2976 surveys of 294 species, including:  
%%   \describe{
%%     \item{\code{yr}}{a numeric vector of years, one for each record (list)}
%%     \item{\code{PA}}{a binary numeric vector of the CoVariate of interest for each record(list)}
%%     \item{\code{Id}}{a numeric vector of unique identifiers, one for each record(list)}
%%     \item{\code{LL}}{a numeric vector indicating the length of each list}
%% }
%%   All remaining columns in the dataset include presence (1)/pseudo-absence(0) data 
%%   in a series of numeric vectors (one for each species in the dataset)
%% }
\details{
The dataframe includes the year in which each list was collected
(\code{yr}), a covariate (in this case "PA" (\code{PA}) indicating
whether or not the list was collected in a proteced area), a unique ID
number for each list (\code{Id}), the length of the list (\code{LL}) and
a series of numeric vectors for each species in the dataset indicating
whether they were recorded (1) or not (0) on each list.
}
\source{
Barnes et al. Unpublished Data (switch data to Judit's before publishing)
}
\references{
  Szabo JK, Vesk PA, Baxter PWJ, Possingham HP (2010) Regional avian
  species declines estimated from volunteer-collected long-term data
  using List Length Analysis. Ecological Applications 20
  (8):2157-2169. doi:10.1890/09-0877.1.

  Barnes at al. Unpublished. The relative impact of protected areas on rainforest endemic
  avifauna of the Australian Wet Tropics. 

  Further guidance on using this package and associated functions and list length analysis in general
  is available at: http://www.edg.org.au/free-tools/listlength.html
}
\examples{
head(LL)
}
\keyword{datasets}