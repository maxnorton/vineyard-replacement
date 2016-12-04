yieldFactors <- read.table("yield-rates.csv", header=TRUE, sep=",")
parameters <- read.table("regional-parameters.csv", header=TRUE, row.names=1, sep=",")
regions <- row.names(parameters)
scenarios <- names(yieldFactors)[2:12]
practices <- c("np", names(parameters[1:3]))
yieldProfiles <- array(dim=c(26,11,5), dimnames=list(yieldFactors$age,scenarios,regions))
costProfiles <- array(dim=c(26,4,5), dimnames=list(yieldFactors$age,practices,regions))

# fill out the age-yield profiles for each region, for each scenario
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

# fill out the age-cost profiles for each region, for each scenario
for (reg in regions) {
  for (prac in practices) {
    for (year in 1:4) {
      costProfiles[year,prac,reg] = parameters[reg,paste("cost", year-1, sep="")] - ifelse(prac=="np",0,parameters[reg,prac])
      #not quite ready yet because the practice cost only applies if year > the scenario year
    }
    for (year in 7:26) {
      costProfiles[year,prac,reg] = parameters[reg,"cost3"]
    }
  }
}

