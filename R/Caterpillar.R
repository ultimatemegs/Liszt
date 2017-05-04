Caterpillar <- function(data, avals = 1:5, nPerPage = 40) {
  ext <- ".pdf"
  x <- avals    
  
  theData <- data

  nSpecies <- nrow(theData)

  main <- xlab <- median <- LC <- UC <- file <- xlim <- NULL
  
  for (i in x){ 
    ax <- paste0("a", i)
    code <- '{
      main <- "A5Model(ALL) ax"       
      xlab <- "ax with credible Intervals" 
      median <- theData$axmedian   
      LC <- theData$ax2.5 
      UC <- theData$ax97.5   
      file <- paste0("ALLaxs", ext)
      xlim <- range(UC, LC, 0)
              }'
    
    eval(parse(text = gsub('ax', ax, code)))

    pdf(file = file, onefile = TRUE)
    n2 <- 0
    
    while(n2 < nSpecies) {
      n1 <- n2+1
      n2 <- min(n2 + nPerPage, nSpecies)
    
      z <- barplot(median[n1:n2], width = 1, horiz = TRUE, col = "grey87", 
                 xlim = xlim, las = 1, 
                 cex.names = 0.5,  space = 1, main = main, 
                 xlab = xlab, ylab = "speciescode", lwd = 1, xpd = NA)                          
    
    segments(LC[n1:n2], z, UC[n1:n2], z, col="red") 
    
    ends <- 0.3 * mean(diff(z))
    segments(LC[n1:n2], z - ends, LC[n1:n2], z + ends, col="red") 
    segments(UC[n1:n2], z - ends, UC[n1:n2], z + ends, col="red") 
    
    box()
    dev.off()  
    }  
  }
}
