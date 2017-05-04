A5ModelFun <-
function(Species, CoVariate, Year, ListLength = rowSums(Species), sp.names = colnames(Species),
           midYear = NULL, n.iter=50000, n.burnin=20000, resultFolder = NULL){

#checks
#      if(length(Year) != length(Species)) {
#          stop("Number of years supplied not equal to number of lists") }
#               
#     #When there is only one species the analysis wont run - Fix...
#      if(length(Species) == 1) 
#         stop("Only one species") }
    
    #Assign default folder name if unspecified
    if(is.null(resultFolder)) resultFolder <- .folder()
    is.folder <- function(file) file.info(file)$isdir
    
    #Assign default midYear if unspecified
    if(is.null(midYear)) midYear <- round(median(unique(Year)))
       
    ## Create a function to generate inits (outside the for loop as it is the same params for every model)
    inits <- function() list(a1=rnorm(1), a2=rnorm(1), a3=rnorm(1), a4=rnorm(1), a5=rnorm(1))
    
    ModelFile <- tempfile()
    LL <- ListLength
    no <- nrow(Species)                     
    my <- midYear
    nyear <- length(unique(Year))
    meanLogLL <- mean(log(ListLength))
    halfyr <- nyear/2
    
    ## Set up a folder for the results
    if(file.exists(resultFolder)) {
      if(!is.folder(resultFolder))
        stop("File ", resultFolder, " exists and is not a folder")
    } else dir.create(resultFolder)
 
#Progress Report
    j <- 1
    tot <- length(sp.names)
    for (i in sp.names) {
      
      cat(i, "\n")
      cat(j, "out of", tot, "\n")
      j <- j+1
    }
    
#####List Length Analysis#####    
    for (i in sp.names) {
      
      ## Define data for each model (we can reuse this object for each model)
      LLAData.1sp <- list(pres=Species[, i], yr=Year, LL=ListLength, PA=CoVariate)
      
      ## Define the JAGS model
      
      cat(sprintf('
                  model
{
                  for (i in 1:%s) {
                  pres[i] ~ dbern(p[i])
                  logit(p[i]) <- a1 +
                  a2 * (log(LL[i]) - %s) +
                  a3 * (yr[i] - %s) +
                  a4 * PA[i] +
                  a5 * (yr[i] - %s) * PA[i]
                  }
                  
                  a1 ~ dnorm(0, 0.0001)
                  a2 ~ dnorm(0, 0.0001)
                  a3 ~ dnorm(0, 0.0001)
                  a4 ~ dnorm(0, 0.0001)
                  a5 ~ dnorm(0, 0.0001)
                  
                  for (k in 1:%s) {
                  predPA[k] <- a1 + a3 * (k - %s) + a4 + a5 * (k - %s)
                  predOUT[k] <- a1 + a3 * (k - %s)
                  diff[k] <- (1 / (1 + exp(-predPA[k]))) - (1 / (1 + exp(-predOUT[k])))  # difference on prob scale
                  }
}', no, meanLogLL, my, my, nyear, halfyr, halfyr, halfyr),
        file=ModelFile)

    ## Run a model and store it in an R object called model_spname (where spname changes with each model)
    assign(sprintf('model_%s', i),
           jags(LLAData.1sp, inits,
                parameters.to.save=c(
                'a1', 'a2', 'a3', 'a4', 'a5', 'predPA', 'predOUT', 'diff'),
                n.iter=n.iter, n.burnin=n.burnin, ModelFile))

    ## Save the current model to a file with the same names
    save(list=sprintf('model_%s', i), file=file.path(resultFolder, sprintf('model_%s.RData', i)))

    ## Delete the current model to save space
    rm(list = sprintf('model_%s', i))
  }

  file.remove(ModelFile)
  return(resultFolder)
}
