yieldFactors <- read.table("yield-rates.csv", header=TRUE, sep=",")
parameters <- read.table("regional-parameters.csv", header=TRUE, row.names=1, sep=",")
regions <- row.names(parameters)
scenarios <- names(yieldFactors)[2:12]
yieldProfiles <- array(dim=c(26,11,5), dimnames=list(yieldFactors$age,scenarios,regions))

# fill out the yield-age profiles for each region, for each scenario
for (reg in regions) {
  for (sce in scenarios) {
    for (year in 1:6) {
      yieldProfiles[year,sce,reg] = (1/100)*yieldFactors[year, sce]*parameters[reg,paste("yield", year-1, sep="")]
    }
    for (year in 7:26) {
      yieldProfiles[year,sce,reg] = (1/100)*yieldFactors[year, sce]*parameters[reg,"yield5"]
    }
  }
}