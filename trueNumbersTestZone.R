#True Numbers intake test space
# HEADER HERE

library(tnum)

source("CensusAPIscript1.R")

acs5Data2019 <- aCS5DataRetriever(2019)

#select just the first row, and first 5 columns...
populationTotalTest <- acs5Data2019[2,(1:5)]

evenFirsterTnum <- tnum.makeObject(
  subject = "2019/new_mexico/county:santa_fe/tract:12-05",
  property = "population:estimated:total",
  value = 5719
)

#generalized subject
# year/stateName/county:countyName/tract:tractNumber

firstTnum <- tnum.makeObject(
  subject = "university:state:montclair",
  property = "rate:graduation",
  value = populationTotalTest[,5]
  unit = "%"
)





# retrieve census data for a single variable.
# the acs data retrieval function but different
#inputs: year, term, variable, geos(could have lots of different implementions)
# to be re-written!
acsCensusDataRetriever <- function(year) {
  #ACS county data for all NCNM is only available for 5 year surveys
  term = "5"  
  # sets the survey name
  surveyName <- acsYearAndTermKey(year,term)
  
  # Fixed variables and location for now, can vary in the future.
  varsToRetrieve <- eMCensusVars
  localsToRetrieve <- acsCountyFips
  
  tractParts <- lapply(localsToRetrieve, getCountyTractData,
                       varsToRetrieve, surveyName)
  #get the data for each geo level, county, state, nation
  countyParts <- lapply(localsToRetrieve, getCountyData, 
                        varsToRetrieve, surveyName)
  
  natPart <- getNationData(varsToRetrieve, surveyName)
  
  statPart <- getNMStateData(varsToRetrieve, surveyName)
  
  # concatenate the different dfs into one
  rawTractData <- bind_rows(tractParts)
  rawCountyData <- bind_rows(countyParts)
  rawData <- gtools::smartbind(rawTractData, rawCountyData, statPart, natPart,  fill = NA)
  
  # switch census variable names for their corresponding labels
  
  
  #labeledData <- renameVariables(rawData, year, term)
  
  return(labeledData)
}
