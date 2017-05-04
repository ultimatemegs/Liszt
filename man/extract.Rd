\name{extract}
\alias{extract}

\title{
	Extract summary data and model outputs for \code{A3Model} and \code{A5Model}  
}
\description{
	Extracts summary data from R objects produced in \code{A3Model} and \code{A5Model} 
	for all species and stores the output in a list "res" 
}
\usage{
extract(dataFolder = NULL)
}
\arguments{
  \item{dataFolder}{
	Enter the name of the folder in which the model output of interest is stored. Model Output must have 
	previously been produced using \code{A3Model}, \code{A5Model} or \code{rjags}.
	The name of this folder will be returned to the R command line after you have run \code{A3Model} or 
	\code{A5Model}. If the folder name is not supplied, the error: "You must specify where you are holding 
	the data!" will be returned.  
}
}
\value{
	 A LIST comprising summary data for each species (1 to n) will be returned.
	 Summary data includes: mean, median, upper and lower confidence intervals and all 
	 parameter values (a1 to a3 for \code{A3Model} and a1 to a5 for \code{A5Model}) 
}
\references{
	Further guidance on using this package and associated functions and list length analysis in general
	is available at: \url{http://www.edg.org.au/free-tools/listlength.html}
	}

\author{
	Megan Barnes <megan.barnes@uq.edu.au>
}

\examples{
\dontrun{	
	### this will only work if you have already run \code{A3Model} or \code{A5Model} and created 
	### a folder in which the resulting model objects are stored. 
	
	A3ModelFun(Species = spp, Year = LL$yr, n.iter = 50, n.burnin = 10, resultFolder= "results")
	extract(datafolder="results") 
}
}
\keyword{ ~kwd1 }
