extract <-
function(dataFolder = NULL)  {
  
  if(is.null(dataFolder)) stop("You must specify where you are holding the data!")
       
  ## Making res2...with median, sd and Rhat... 
  mods <- dir(path = dataFolder, pattern = "^model_.*\\.RData$")
  if(length(mods) == 0) stop("No models found!")
       
##  this fishes things out of this object... I want objects a1-a5 and columnsc(...)
##  lapply(name of what i want x to be, function(x) {write function})
  
  nams <- sub("^model_", "", sub("\\.RData$", "", mods))
  res <- structure(vector("list", length(mods)), names = nams)
  
  for(i in 1:length(mods)) {
    x <- mods[i]
    load(file.path(dataFolder, x))
    x <- sub("\\.RData$", "", x)
    val <- get(x)$BUGSoutput$summary
    rows <- grep("^a", rownames(val))
    cols <- setdiff(colnames(val), "n.eff")
    val <- val[rows, cols]
    val2 <- get(x)$BUGSoutput$median[rows]
    res[[ nams[i] ]] <- cbind(val, median = unlist(val2))
    rm(val, val2, list = x)
  }
  res  
}
