yieldFactors <- read.table("yield-rates.csv", header=TRUE, sep=",")
parameters <- read.table("regional-parameters.csv", header=TRUE, row.names=1, sep=",")
regions <- row.names(parameters)
scenarios <- names(yieldFactors)[2:12]
practices <- c("np", names(parameters[1:3]))
yieldProfiles <- array(dim=c(26,11,5), dimnames=list(yieldFactors$age,scenarios,regions))
costProfiles <- array(dim=c(26,11,4,5), dimnames=list(yieldFactors$age,scenarios,practices,regions))
netReturns <- array(dim=c(26,11,4,5), dimnames=list(yieldFactors$age,scenarios,practices,regions))
discNetReturns <- array(dim=c(26,11,4,5), dimnames=list(yieldFactors$age,scenarios,practices,regions))
cumDNR <- array(dim=c(26,11,4,5), dimnames=list(yieldFactors$age,scenarios,practices,regions))

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
  for (sce in scenarios) {
    for (prac in practices) {
      for (year in 1:26) {
        costProfiles[year,sce,prac,reg] = parameters[reg,ifelse(year-1<4,paste("cost", year-1, sep=""),"cost3")] - ifelse(prac=="np" | sce=="healthy" | sce=="infected", 0, ifelse(year-1<as.numeric(substr(sce,5,nchar(sce))),0,parameters[reg,prac]))
      }
    }
  }
}

# fill out the netReturns array for each region, for each scenario, for each practice, for each year
for (reg in regions) {
  for (sce in scenarios) {
    for (prac in practices) {
      for (year in 1:26) {
        netReturns[year,sce,prac,reg] = yieldProfiles[year,sce,reg]*parameters[reg,"price"] - costProfiles[year,sce,prac,reg]
      }
    }
  }
}

# discount the netReturns tables. discount rate is hard-coded here.
for (reg in regions) {
  for (sce in scenarios) {
    for (prac in practices) {
      for (year in 1:26) {
        discNetReturns[year,sce,prac,reg] = netReturns[year,sce,prac,reg]/(1.03^(year-1))
      }
    }
  }
}


# sum the net returns to make the cumDNR tables: cumDNR = present returns + prev year's returns
for (reg in regions) {
  for (sce in scenarios) {
    for (prac in practices) {
      for (year in 1:26) {
        cumDNR[year,sce,prac,reg] = discNetReturns[year,sce,prac,reg] + ifelse(year-1>0, cumDNR[year-1,sce,prac,reg],0)
      }
    }
  }
}

