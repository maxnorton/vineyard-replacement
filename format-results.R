produce <- function(yr,prac,reg,precalc) {
  output <- data.frame(matrix(" ", nrow=23, ncol=8), stringsAsFactors=FALSE)
  practiceName <- switch(prac, "dp"="Delayed pruning", "hp"="Topsin", "dbp"="Double pruning")
  regionName <- switch(reg, "napa"="Napa (4)", "nsj"="Northern San Joaquin (11)", "cc"="San Luis Obispo (8)", "lake"="Lake (2)", "son"="Sonoma (3)")
  for (i in c(1,9,17)) {
    output[i,1] <- "Scenario: Replace after first year of negative returns"
    output[i+1,1] <- paste("Preventative practice: ",practiceName, " ", sub("Y ([35(10)])","Y\\1",paste("Y",as.character(yr),"")), switch(as.character(i), "1"=" 50", "9"=" 25", "17"=" 75"), "% DCE","")
    output[i+2,1] <- paste("Variety: Cabernet Sauvignon, ", regionName, "")
    output[i,3] <- "Replace after year"
    #output[i+1,3] <- GET cycleLengths
    output[i+2,3] <- "Lifecycle start year (discount base)"
    output[i+2,4] <- "0"
    #output[i+2,5] <- ifelse(precalc, GET cycleStarts, "=FORMULA")
    #output[i+2,6] <- ifelse(precalc, GET cycleStarts, "=FORMULA")
    #output[i+2,7] <- ifelse(precalc, GET cycleStarts, "=FORMULA")
    output[i+3,3] <- "Lifecycle end year"
    #output[i+3,4] <- ifelse(precalc, GET cycleEnds, "=FORMULA")
    #output[i+3,5] <- ifelse(precalc, GET cycleEnds, "=FORMULA")
    #output[i+3,6] <- ifelse(precalc, GET cycleEnds, "=FORMULA")
    #output[i+3,7] <- ifelse(precalc, GET cycleEnds, "=FORMULA")
    output[i+4,3] <- "Lifecycle CNR disc to lifecycle BY"
    #output[i+4,4] <- ifelse(precalc, GET cycleCNRs, "=FORMULA")
    #output[i+4,5] <- ifelse(precalc, GET cycleCNRs, "=FORMULA")
    #output[i+4,6] <- ifelse(precalc, GET cycleCNRs, "=FORMULA")
    #output[i+4,7] <- ifelse(precalc, GET cycleCNRs, "=FORMULA")
    output[i+5,3] <- "LCNR disc to year 0"
    #output[i+4,4] <- ifelse(precalc, GET cycleDCNRs, "=FORMULA")
    #output[i+4,5] <- ifelse(precalc, GET cycleDCNRs, "=FORMULA")
    #output[i+4,6] <- ifelse(precalc, GET cycleDCNRs, "=FORMULA")
    #output[i+4,7] <- ifelse(precalc, GET cycleDCNRs, "=FORMULA")
    output[i+2,8] <- "final lifecycle length"
    #output[i+3,8] <- ifelse(precalc, GET shortCycleLengths, "=FORMULA")
    output[i+4,8] <- "Total discounted net returns:"
    #output[i+5,8] <- ifelse(precalc, GET totalDNRs, "=SUM(DROW:FROW)")
  }
  return(output)
}