for (reg in regions) {
  for (yr in c("3","5","10")) {
    for (prac in c("dp","hp","dbp")) {
      theseresults <- produce(as.numeric(yr), prac, reg, TRUE)
      assign(paste(reg,yr,prac,sep=""), theseresults)
    }
  }
  theseresults <- produceInfected(reg,TRUE)
  assign(paste(reg,"-infected",sep=""), theseresults)
}