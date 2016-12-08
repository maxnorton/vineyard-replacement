for (reg in regions) {
  for (yr in c("3","5","10")) {
    for (prac in c("dp","hp","dbp")) {
      theseresults <- produce(as.numeric(yr), prac, reg, FALSE)
      write.xlsx(theseresults, paste(reg,yr,prac,".xlsx",sep=""))
    }
  }
  theseresults <- produce(0,"np",reg,FALSE)
  write.xlsx(theseresults, paste(reg,"-infected.xlsx",sep=""))
}