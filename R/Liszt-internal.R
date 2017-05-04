.folder <-
function(tag = "ListLengthAnalysis") {
  paste(gsub("[^0-9]", "_", as.character(strptime(date(), "%a %b %d %H:%M:%S %Y"))), tag, sep="_")
}
