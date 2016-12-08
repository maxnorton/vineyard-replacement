column <- function(number) {
  letter <- switch(as.character(number),"1"="A","2"="B","3"="C","4"="D","5"="E","6"="F","7"="G","8"="H")
  return(letter)
}

produce <- function(yr,prac,reg,precalc) {
  output <- data.frame(matrix(" ", nrow=23, ncol=8), stringsAsFactors=FALSE)
  practiceName <- switch(prac, "dp"="Delayed pruning", "hp"="Topsin", "dbp"="Double pruning")
  regionName <- switch(reg, "napa"="Napa (4)", "nsj"="Northern San Joaquin (11)", "cc"="San Luis Obispo (8)", "lake"="Lake (2)", "son"="Sonoma (3)")
  baseRow <- switch(reg, "napa"=24, "nsj"=48, "cc"=72, "lake"=96, "son"=120)
  
  for (i in c(1,9,17)) {
    efficacy <- switch(as.character(i), "1"="50", "9"="25", "17"="75")
    sce <- paste("e", efficacy, "y", as.character(yr), sep="")
    
    output[i,1] <- "Scenario: Replace after first year of negative returns"
    output[i+1,1] <- paste("Preventative practice: ",practiceName, " ", "Y", as.character(yr), efficacy, "% DCE", sep="")
    output[i+2,1] <- paste("Variety: Cabernet Sauvignon, ", regionName, sep="")
    output[i,3] <- "Replace after year"
    output[i+1,3] <- cycleLengths[sce,prac,reg]
    output[i+2,3] <- "Lifecycle start year (discount base)"
    output[i+2,4] <- "0"
    for (cycle in 2:sum(!is.na(cycleStarts[,sce,prac,reg]))) {
      output[i+2,3+cycle] <- ifelse(precalc, cycleStarts[cycle,sce,prac,reg], paste("=",column(3+cycle-1),baseRow+i+2+1,"+1",sep=""))
    }
    output[i+3,3] <- "Lifecycle end year"
    for (cycle in 1:sum(!is.na(cycleEnds[,sce,prac,reg]))) {
      output[i+3,3+cycle] <- ifelse(precalc, cycleEnds[cycle,sce,prac,reg], paste("=IF(",column(3+cycle),baseRow+i+3-1,"+$C",baseRow+i+1,", ",column(3+cycle),baseRow+i+3-1,"+$C",baseRow+i+1, ", 50)",sep=""))
    }
    output[i+4,3] <- "Lifecycle CNR disc to lifecycle BY"
    for (cycle in 1:sum(!is.na(cycleCNRs[,sce,prac,reg]))) {
      output[i+4,3+cycle] <- cycleCNRs[cycle,sce,prac,reg]
    }
    output[i+5,3] <- "LCNR disc to year 0"
    for (cycle in 1:sum(!is.na(cycleDCNRs[,sce,prac,reg]))) {
      output[i+5,3+cycle] <- ifelse(precalc, cycleDCNRs[cycle,sce,prac,reg], paste("=",column(3+cycle),baseRow+i+5-1,"/(1+assumptions!$B$1)^",column(3+cycle),baseRow+i+5-3,sep=""))
    }
    output[i+2,8] <- "final lifecycle length"
    output[i+3,8] <- ifelse(precalc, ifelse(!is.na(shortCycleLengths[sce,prac,reg]), shortCycleLengths[sce,prac,reg], cycleLengths[sce,prac,reg]), paste("=",column(3+sum(!is.na(cycleStarts[,sce,prac,reg]))),baseRow+i+3,"-",column(3+sum(!is.na(cycleStarts[,sce,prac,reg]))),baseRow+i+3-1,sep=""))
    output[i+4,8] <- "Total discounted net returns:"
    #output[i+5,8] <- ifelse(precalc, GET totalDNRs, "=SUM(DROW:FROW)")
    output[i+5,8] <- ifelse(precalc, totalDNRs[sce,prac,reg], paste("=SUM(D",baseRow+i+5,":F",baseRow+i+5,")",sep=""))
  }
  return(output)
}