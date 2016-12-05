yieldFactors <- read.table("yield-rates.csv", header=TRUE, sep=",")
parameters <- read.table("regional-parameters.csv", header=TRUE, row.names=1, sep=",")
regions <- row.names(parameters)
scenarios <- names(yieldFactors)[2:12]
practices <- c("np", names(parameters[1:3]))
plantings <- c(1,2,3,4)

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
        costProfiles[year,sce,prac,reg] = parameters[reg,ifelse(year-1<4,paste("cost", year-1, sep=""),"cost3")] + ifelse(prac=="np" | sce=="healthy" | sce=="infected", 0, ifelse(year-1<as.numeric(substr(sce,5,nchar(sce))),0,parameters[reg,prac]))
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

horizon <- 51
cycleLengths <- array(dim=c(10,4,5), dimnames=list(scenarios[2:11],practices,regions))
shortCycleLengths <- array(dim=c(10,4,5), dimnames=list(scenarios[2:11],practices,regions))
cycleStarts <- array(dim=c(4,10,4,5), dimnames=list(plantings,scenarios[2:11],practices,regions))
cycleEnds <- array(dim=c(4,10,4,5), dimnames=list(plantings,scenarios[2:11],practices,regions))

# identify cycle length for each region, for each scenario, for each practice as year where cumDNR falls below prev. year, unless we get to year 26
for (reg in regions) {
  for (sce in scenarios[2:11]) {
    for (prac in practices) {
      year <- 5
      while (cumDNR[year,sce,prac,reg]>=cumDNR[year-1,sce,prac,reg] & year<26) {
        year <- year + 1
      }
      cycleLengths[sce,prac,reg] <- year-1
    }
  }
}

# generate list of cycle start years for each region, scenario, practice
for (reg in regions) {
  for (sce in scenarios[2:11]) {
    for (prac in practices) {
      cycleStarts[1,sce,prac,reg] <- 0
      cycle <- 2
      while (cycleStarts[cycle-1,sce,prac,reg] + cycleLengths[sce,prac,reg] < 50) {
        cycleStarts[cycle,sce,prac,reg] <- cycleStarts[cycle-1,sce,prac,reg] + cycleLengths[sce,prac,reg] + 1
        cycle <- cycle + 1
      }
    }
  }
}

# generate list of cycle end years for each region, scenario, practice
for (reg in regions) {
  for (sce in scenarios[2:11]) {
    for (prac in practices) {
      cycleEnds[1,sce,prac,reg] <- cycleLengths[sce,prac,reg]
      cycle <- 2
      while (cycleStarts[cycle,sce,prac,reg] + cycleLengths[sce,prac,reg] < 50) {
        cycleEnds[cycle,sce,prac,reg] <- cycleStarts[cycle,sce,prac,reg] + cycleLengths[sce,prac,reg]
        cycle <- cycle + 1
      }
      if(cycleEnds[cycle-1,sce,prac,reg]!=50) cycleEnds[cycle,sce,prac,reg] <- 50
    }
  }
}

# generate lengths of last (incomplete/short) cycles, by region, scenario, practice
for (reg in regions) {
  for (sce in scenarios[2:11]) {
    for (prac in practices) {
      ifelse(cycleEnds[sum(!is.na(cycleEnds[,sce,prac,reg])),sce,prac,reg] - cycleStarts[sum(!is.na(cycleEnds[,sce,prac,reg])),sce,prac,reg] != cycleLengths[sce,prac,reg], shortCycleLengths[sce,prac,reg] <- cycleEnds[sum(!is.na(cycleEnds[,sce,prac,reg])),sce,prac,reg] - cycleStarts[sum(!is.na(cycleEnds[,sce,prac,reg])),sce,prac,reg], shortCycleLengths[sce,prac,reg] <- NA)
    }
  }
}