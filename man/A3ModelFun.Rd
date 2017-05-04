\name{A3ModelFun}
\alias{A3ModelFun}
\title{
List Length Analysis (A3 Model) 
}
\description{
These functions conduct list length analysis in a bayesian framework by interfacing with the rjags package. 
}
\usage{A3ModelFun(Species, Year, ListLength = rowSums(Species), sp.names =
           colnames(Species), midYear, n.iter = 50000, n.burnin = 20000, 
           resultFolder)      
}

\arguments{
  \item{Species}{
    Data frame for presence/absence (list) data, with rows corresponding to visit,
    and columns to Species. By default, column names are used as
    the species names.
}
  \item{Year}{
    Numeric vector giving the calendar year for each record (i.e. each individual survey/list). 	Must correspond to the rows of \code{Species}. Model will not run if years are not supplied 
    for each record.
}
  \item{ListLength}{
    Numeric vector giving the lengths of each recorded list; by default
    equal to the row sums of \code{Species}.
}
  \item{sp.names}{
    Character string vector of length the same as the number of columns in
    \code{Species}, giving the species names.  By default the
    column names of \code{Species}.
}
  \item{midYear}{
    Single numeric value giving a reference year near the central year of
    the recording period. Must be integer. By default the closest year to 
    the median of the years for which records are supplied.
}
  \item{n.iter}{
    Number of MCMC iterations to be used. By default, 50000. This is quite slow, 
    so we suggest selecting an arbitrary low number (e.g. 10, 100, 1000) for testing.  
}
  \item{n.burnin}{
    Number of burn-in iterations for the MCMC computations. By default, 20000.
    Must be less than \code{n.iter}. Usually, around 1/3 of the number of MCMC 
    iterations (\code{n.iter}) would be selected.   
}
  \item{resultFolder}{
    The name to be used for the sub-folder of the working directory where
    the results are to be stored.  By default, \code{resultFolder}
    with a time-stamp. If this folder currently exists, it will
    be re-used; if not, it will be created. If resultFolder is NULL, 
    folder name will be supplied: "ListLengthAnalysisResults" with a timestamp.  
  }
}
\details{
  These are linking functions that generate the necessary code to conduct list length 
  analysis in a bayesian framework using the \code{rjags} package with the data supplied by the 
  user. Two models, called A3 and A5, are supplied, and may be used directly, or used as templates 
  if other versions are required. The A3 Model is a standard list length analysis {Szabo et al 	
  2010} and the A5 model conducts relative impact analysis {Barnes et al. Unpublished}.   
}

\value{
  The package returns the name of the result folder used as a character string, 
  at the R command line.  

  The function is called for its side-effects, which are a series of
  binary R objects, held in the result folder, giving the results of the
  MCMC computations for each of the species in the \code{Species}
  argument.
  
  Further functions may be supplied which perform
  post-processing operations on these generated R objects, 
  including those supplied in this package: \code{extract} and \code{Caterpillar}  
}
\references{
  Szabo JK, Vesk PA, Baxter PWJ, Possingham HP (2010) Regional avian
  species declines estimated from volunteer-collected long-term data
  using List Length Analysis. Ecological Applications 20
  (8):2157-2169. doi:10.1890/09-0877.1.

  Barnes at al. Unpublished. The relative impact of protected areas on rainforest endemic
  avifauna of the Australian Wet Tropics. 

  Further guidance on using this package and associated functions and list length analysis in general
  is available at: \url{http://www.edg.org.au/free-tools/listlength.html}
}

\author{
	Megan Barnes <megan.barnes@uq.edu.au>
}
\examples{
\dontrun{
data(LL)
spp <- LL[,-c(1:2)]
A3ModelFun(Species = spp, Year = LL$yr, n.iter = 50, n.burnin = 10)
}
}
\keyword{ models }
\keyword{ multivariate }