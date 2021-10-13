#title: ACS data True Numberfier
#author: Aidan O'Hara
#date: 10/05/2021

library(tnum)
library(stringr)

source("CensusAPIscript1.R")

# EXAMPLE TRUE NUMBERS #
# LEARNING ABOUT TNUMS  #
# evenFirsterTnum <- tnum.makeObject(
#   subject = "2019/new_mexico/county:santa_fe/tract:12-05", #old
#   property = "population:estimated:total", #old
#   value = 5719
# )

# santaFeTnum <- tnum.makeObject(
#   subject = "new_mexico/county:santa_fe/tract:000200",
#   property = "some_other_race:two_plus_races/population:estimated",  #old
#   value = 17,
#   tags = c("2019/acs/acs5", "group_B02001") #old
# )

# testTnum <- tnum.makeObject(
#   subject = "United_states",
#   property = "haha", #fake
#   value = 17, #unknown
#   tags = c("eeek","aaack"), #fake
#   error = 5
# )

# random variable for value above
# value = 17
# generalized subject
# year/stateName/county:countyName/tract:tractNumber
# Fold the margin of error data into each true numbers about the data point in Q



# This oddly simple function always underwhelms and surprises me.  This process,
#   is gonna get a lot more hectic before we make it out of the woods. 
# oneTrueNumberPlease <- function(subjectValue, property){
#   aTNum <- tnum.makeObject(
#     subject = unlist(subjectValue[1]),
#     property = property,
#     value = unname(unlist(subjectValue[2])),
#     tags = c("2019/acs/acs5", "group:B02001") # generalized later
#   )
#   return(aTNum)
# }

# Acs census data retriever, specifically just has race data. 
#    used for building first tnums, 
testSFGroupB02001 <- acsDataRetriever(2019, 5, "state:35",
                                      "049", "B02001")
# Isolate the columns that will pertain to a single set of true numbers, i.e.
#   an estimate column, and a margin of error column, alongside geo info.
doubleTest <- testSFGroupB02001[c(1:5,7,
                                  (grep("us;",colnames(testSFGroupB02001))))]

# HELPER FUNCTIONS #
# Function that places ":" between input strings
colonSep <- function(stringsToCombine) {
  paste(stringsToCombine, collapse = ":")
}

# Function that places "/" between input strings
slashSep <- function(stringsToCombine) {
  paste(stringsToCombine, collapse = "/")
}

# "Census Tract 8, Santa Fe County, New Mexico" -> "santa_fe"
# in an effort to avoid building a reference table to go from county-fips to
# county name, I'm just parsing the name out of the NAME column.
generateCountyString <- function(nameString) {
   parts <- unlist(str_split(nameString, ", "))
   countyName <- parts[grepl("County", parts)]
   partsAgain <- unlist(str_split(countyName, " "))
   noCounty <- partsAgain[!grepl("County", partsAgain)]
   combined <- paste(noCounty, collapse = '_')
   lowered <- str_to_lower(combined)
   return(lowered)
}

#https://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2019_Instructions_for_Stat_Testing_ACS.pdf
convertACSMOEtoSE <- function(acsMOE) {
  if(acsMOE > 0) {
    return(acsMOE/1.645)
  } else {
    return(0)
  }
}

# MAKE A TRUE NUMBER  #
makeTrueNumber <- function(aDataFrameRow, propertyClause, estimateColumnIndex,
                           mOEColumnIndex, listOfTags, unitLabel) {
  # generate a TNUM-county-string
  countyTitle <- generateCountyString(aDataFrameRow['NAME'])
  # test column entries for most specific geography NAME available.
  if (is.na(aDataFrameRow['state'])) {
    subjecter <- ("united_states")
  } else if (is.na(aDataFrameRow['county'])) {
    subjecter <- ("new_mexico")
  } else if (is.na(aDataFrameRow['tract'])) {
    subjecter <- (slashSep(c("new_mexico",
                             colonSep(c("county", countyTitle)))))
  } else {
    subjecter <- (slashSep(c("new_mexico",
                             colonSep(c("county", countyTitle)),
                             colonSep(c("tract", aDataFrameRow['tract'])))))
  }
  
  # set the value from the DF
  valueNumber <- as.integer(aDataFrameRow[estimateColumnIndex])
  
  # tests to be sure everything got set just right.
  #print(subjecter)
  #print(property)
  #print(valueNumber)
  #print(listOfTags)
  #print("in make function")
  #print(propertyClause)
  # Make a single true number with the assigned value
  aNum <- tnum.makeObject(
    subject = subjecter,
    error = convertACSMOEtoSE(as.integer(aDataFrameRow[mOEColumnIndex])),
    property = propertyClause,
    valueNumber,
    unit = unitLabel,
    tags = listOfTags
  )
  # print test to be sure aNum has the right structure
  # print(aNum)
  return(aNum)
}

# function for user input to parse the variable name into a TNUM property clause
userSetProperty <- function(columnNames) {
  print(columnNames)
  print("example: race:some_other:two_plus_races/population:estimated")
  my.property <- readline(prompt = paste("property clause: "))
  return(my.property)
}

# rips up the "us;year:survey:etc..." into a list of usable TNUM tags
extractTaggage <- function(columnStringWithTags) {
  bits <- str_split(columnStringWithTags, ";")
  lessBits <- tail(unlist(bits), n = -1)
  commaSplitTest <- paste(lessBits, collapse = ",")
  #print(commaSplitTest)
  return(commaSplitTest)
}

# Recieves column names, removes the stringy bits and chops off the tail of the 
#     variable code, removes the duplicates and then returns a list of numbered
#     group variable code names, present in the original list.
# "Estimate!!Total:!!...other race/B02001_009E" -> "B02001_009" 
makeListOfPresentVariableCodes <- function(variableNamesList) {
  refList <- lapply(variableNamesList, str_split, "/")
  getJustVarCode <- function(listOfNameStrings) {
    return(unlist(listOfNameStrings)[2])
  }
  justVarCodes <- lapply(refList, getJustVarCode)
  stripEm <- str_sub(unlist(justVarCodes), 1, nchar(justVarCodes)-1) 
  return(unique(stripEm))
}

### Data-frame slicer-upper extraordinaire ###
# True Numbers ingestion is most easily facilitated by working with single
#    "property" variables.  In ACS's case, 2 matching columns, one for est.pop.
#    the other for m.o.e. can be combined into a single True Number. 
# This function takes input as an acsData-Frame created by the acsDataRetriever,
#  and returns a list of Partial DF's containing matching sets of ACS columns.
#EX#
# doubleTest <- testSFGroupB02001[c(1:5,7,
#                                   (grep("us;",colnames(testSFGroupB02001))))]

# columns 1:4 for state. county, NAME. 5 and 7 are Pop.Est. and MOE data.  
#    Finally, use grep to locate the "us;..." column, note that the end of the 
#     "us;..." string varies per DF.

makePartialsDFs <- function(acsData) {
  # Make A list of the names columns that will be in every DF
  # state, county, tract, NAME, us;..., 
  consistentColumnNames <- c(colnames(acsData[grep("us;", colnames(acsData))]),
                             "state","county","tract","NAME")
  #   Select the non-consistent column names, create a list
  variableNameList <- unique(colnames(acsData)[! colnames(acsData) %in%
                                                 consistentColumnNames])
  # Pass the names to helper function for dissection.
  codes <- makeListOfPresentVariableCodes(variableNameList)

  # For each pair of Variable/MOE in the above list, create a new partial DF,
  # containing the pertinent columns, add it too a list.
  # For each code, build a partial DF, with consistentColumns, and matching
    # column names, use grep or so to make the selection.
  
  # Local Help Function #
  # retrieves the matching column indexes, then uses the corresponding names
  #     to contruct a partialDF with only consistentColumns, and a pair of 
  #     matching est.pop./moe columns.
  selectPartialDF <- function(singleCodeString) {
    codeColumns <- colnames(acsData)[grep(singleCodeString, 
                                                    colnames(acsData))]
    singlePartialDF <- acsData[,c(consistentColumnNames, 
                                 codeColumns)]
    return(singlePartialDF)
  }
  # Lapply the helper function, to the list of present codes
  listOfPartialDFs <- lapply(codes, selectPartialDF)
  # return aListOfPartialDF
  return(listOfPartialDFs)
}



### True Number Workstation ###
# Next step along the path to True Numbers
# Uses a single input generated by makePartialDFs
# Outputs a list of True Numbers made from the Partial DF
makeTNumsFromPartialDF <- function(singleVariableDataFrame) {
  # First things first, user needs to set the property, call userSetProperty
  #    Least operable part of the code so far, very inefficient
  #propertyClause <- userSetProperty(colnames(singleVariableDataFrame))
  estimateColumnLabel <- colnames(
    singleVariableDataFrame[grep("Estimate",
                                 colnames(singleVariableDataFrame))])
  propertyClauseRow <- getPropertyFromCode(estimateColumnLabel, 
                                        columnPropertyKeyWithTags)
  #print(propertyClauseRow$columnNamesKeyCode)
  # Next, set the tags, using extractTaggage, grep the "us;" column for the 
  #   functions input
  listOfTags <- extractTaggage(colnames(singleVariableDataFrame)[
    grep("us;", colnames(singleVariableDataFrame))])
  #print(listOfTags)
  
  if (!is.na(propertyClauseRow$columnTags)) {
    listOfTags <- paste(listOfTags, propertyClauseRow$columnTags, sep = ",")
  }
  unitLabel <- NA
  if (!is.na(propertyClauseRow$unitTags)) {
    unitLabel <- propertyClauseRow$unitTags
    #print(unitLabel)
  }
  listOfTags <- paste(listOfTags, ingestionTag, sep = ",")
  # retrieve and store an index for the est. and m.o.e. columns.
  estimateColumnIndex <- grep("Estimate", colnames(singleVariableDataFrame))
  mOEColumnIndex <- grep("Margin", colnames(singleVariableDataFrame))
  
  # propertyClause <- extractProperty(colnames(singleVariableDataFrame)
  #                                   [estimateColumnIndex],listOfTags)
  
  # Build theNUMS, these columns very own list of true numbers
  #    Calling upon makeTrueNumber from earlier with each row of the partial DF
  # One True Number per row, all in a big list.
  
  #consider using Append.
  theNums <- list()
  theNUMS <- as.list(apply(singleVariableDataFrame,MARGIN = 1, makeTrueNumber, 
                    propertyClauseRow$columnNamesKeyCodes, estimateColumnIndex, 
                    mOEColumnIndex, listOfTags, unitLabel, simplify = FALSE))
  return(theNUMS)
}

# extractProperty <- function(estimateColumnName, tags) {
#   specifier <- #strip the column name "race"
# }

# columnPaintThinner <- function(columnString) {
#   parts <- strsplit(columnString, '[!/]')
#   parts <- parts[grep(" ", parts)]
#   return(parts)
# }

# TESTING ZONE #
# Make some new data frames, turn them into true numbers, rejoice.
testAllGeosPop2019 <- acsDataRetriever(2019, 5, "state:35",
                                       acsCountyFips, "B01003")

testALLNums <- lapply(makePartialsDFs(testAllGeosPop2019),
                      makeTNumsFromPartialDF)
testNum <- testALLNums[[1]][99] 
# error "-555555555" ->
# A '*****' entry in the margin of error column indicates that the estimate is 
# controlled. A statistical test for sampling variability is not appropriate.
# https://www.census.gov/data/developers/data-sets/acs-1year/notes-on-acs-(...)
#   (...)estimate-and-annotation-values.html
totalPopulationAlone2019 <- acsDataRetriever(2019, 5, "state:35",
                                             acsCountyFips, "B01003")
totalPopulationAlone2019TNums <- lapply(
  makePartialsDFs(totalPopulationAlone2019),
  makeTNumsFromPartialDF)

perCapitaIncome2019 <- acsDataRetriever(2019, 5, "state:35",
                                        acsCountyFips, "B19301")
# not income:per_capita/dollars:estimated
#  actually income:per_capita:estimated
perCapitaIncome2019TNums <- lapply(makePartialsDFs(perCapitaIncome2019),
                                   makeTNumsFromPartialDF)
perCapitaIncome2019TNums[[1]][118]
perCapitaIncome2019TNums[[1]][117]


povertyStatusByAge2019 <- acsDataRetriever(2019, 5, "state:35",
                                           acsCountyFips, "B17020")
povertyStatusByAge2019TNums <- lapply(makePartialsDFs(povertyStatusByAge2019),
                                      makeTNumsFromPartialDF)

populationByRace2019 <- acsDataRetriever(2019, 5, "state:35",
                                         acsCountyFips, "B02001")
populationByRace2019TNums <- lapply(makePartialsDFs(populationByRace2019),
                                    makeTNumsFromPartialDF)

citizenshipStatus2019 <- acsDataRetriever(2019, 5, "state:35",
                                          acsCountyFips, "B05001")
citizenshipStatus2019TNums <- lapply(makePartialsDFs(citizenshipStatus2019),
                                     makeTNumsFromPartialDF)

financeData <- acsDataRetriever(2019, 5, "state:35",
                                acsCountyFips, "B19037")

# Retrieve 5 Group Variables detailing demographics


#B01003
totalPopulationAlone2019 <- acsDataRetriever(2019, 5, "state:35",
                                             acsCountyFips, "B01003")
totalPopulationAlone2011 <- acsDataRetriever(2011, 5, "state:35",
                                             acsCountyFips, "B01003")
totalPopulationAlone2019TNums <- lapply(
  makePartialsDFs(totalPopulationAlone2019),
  makeTNumsFromPartialDF)
#B02001
populationByRace2019 <- acsDataRetriever(2019, 5, "state:35",
                                         acsCountyFips, "B02001")
populationByRace2011 <- acsDataRetriever(2011, 5, "state:35",
                                         acsCountyFips, "B02001")
populationByRace2019TNums <- lapply(makePartialsDFs(populationByRace2019),
                                    makeTNumsFromPartialDF)
#B19301
perCapitaIncome2019 <- acsDataRetriever(2019, 5, "state:35",
                                        acsCountyFips, "B19301")
perCapitaIncome2011 <- acsDataRetriever(2011, 5, "state:35",
                                        acsCountyFips, "B19301")
# not income:per_capita/dollars:estimated
#  actually income:per_capita:estimated
perCapitaIncome2019TNums <- lapply(makePartialsDFs(perCapitaIncome2019),
                                   makeTNumsFromPartialDF)
#B17020
povertyStatusByAge2019 <- acsDataRetriever(2019, 5, "state:35",
                                           acsCountyFips, "B17020")
povertyStatusByAge2013 <- acsDataRetriever(2013, 5, "state:35",
                                           acsCountyFips, "B17020")
povertyStatusByAge2019TNums <- lapply(makePartialsDFs(povertyStatusByAge2019),
                                      makeTNumsFromPartialDF)
#B23025
employmentStatus2019 <- acsDataRetriever(2019, 5, "state:35",
                                           acsCountyFips, "B23025")
employmentStatus2011 <- acsDataRetriever(2011, 5, "state:35",
                                         acsCountyFips, "B23025")
employmentStatus2011TNums <- lapply(makePartialsDFs(employmentStatus2011),
                                      makeTNumsFromPartialDF)

tNumsFromDataSet <- function(dataSet) {
  return (lapply(makePartialsDFs(dataSet),(makeTNumsFromPartialDF)))
}

doubleFlatten <- function(listOflistOfTnums) {
  return(purrr::flatten(purrr::flatten(listOflistOfTnums)))
}

yearsForFirstDataSet <- c(2011,2013,2015,2017,2019)
ingestionTag <<- "ingest:acsGroups1"
# Construct Data Frames
listOfTotalPopulationDF <- lapply(yearsForFirstDataSet, acsDataRetriever,
                                  5, "state:35", acsCountyFips, "B01003")
listOfRacePopulationDF <- lapply(yearsForFirstDataSet, acsDataRetriever,
                                 5, "state:35", acsCountyFips, "B02001")
listOfPerCapIncomeDF <- lapply(yearsForFirstDataSet, acsDataRetriever,
                               5, "state:35", acsCountyFips, "B19301")
listOfPovertyStatusDF <- lapply(c(2013,2015,2017,2019), acsDataRetriever,
                                5, "state:35", acsCountyFips, "B17020")
listOfEmploymentStatusDF <- lapply(yearsForFirstDataSet, acsDataRetriever,
                                   5, "state:35", acsCountyFips, "B23025")

# Convert to Tnums
totalPopulationTnums <- lapply(listOfTotalPopulationDF, tNumsFromDataSet)
racePopulationTnums <- lapply(listOfRacePopulationDF, tNumsFromDataSet)
perCapIncomeTnums <- lapply(listOfPerCapIncomeDF, tNumsFromDataSet)
povertyStatusTnums <- lapply(listOfPovertyStatusDF, tNumsFromDataSet)
employmentStatusTnums <- lapply(listOfEmploymentStatusDF, tNumsFromDataSet)


# Collapse
allPopTnums <- doubleFlatten(totalPopulationTnums)
#populationByRace
allRaceTnums <- doubleFlatten(racePopulationTnums)
#perCapitaIncome
allPerCapTnums <- doubleFlatten(perCapIncomeTnums)
#povertyStatusByAge
allPovTnums <- doubleFlatten(povertyStatusTnums)
#employmentStatus
allEmpTnums <- doubleFlatten(employmentStatusTnums)


firstTrueNumbersACSDataSet <- tnum.objectsToDf((c(allPopTnums,
                                                  allRaceTnums,
                                                  allPovTnums,
                                                  allPerCapTnums,
                                                  allEmpTnums)))

# remove some duplicate tnums #
# need to remove: B02001_001, B17020_001, B23025_001.
# filter the set by population:estimated
duplicateTnums <- filter(firstTrueNumbersACSDataSet, 
                         firstTrueNumbersACSDataSet$property == 
                           'population:estimated',
                         !grepl("B01003", firstTrueNumbersACSDataSet$tags))
firstArchive <- dplyr::anti_join(firstTrueNumbersACSDataSet, duplicateTnums)

tnum.authorize(ip = "10.231.32.101")


write.csv(firstTrueNumbersACSDataSet,
        "C:\\RStudioProjects\\ncnmeddDataTool\\firstTrueNumbersACSDataSet.csv", 
          row.names = FALSE)


testDF <- tnum.objectsToDf(allPopTnums)
View(testDf)

tn <- list()
tn[[1]] <- allPopTnums[[116]]
tn[[2]] <- allRaceTnums[[117]]
tn[[3]] <- allPovTnums[[47]]
tn[[4]] <- allPerCapTnums[[500]]
tn[[5]] <- allEmpTnums[[3]]


# The following are the framework for codifying the properties
#   2011 column name, minding poverty status by age
columnNamesFirst <- c(colnames(totalPopulationAlone2011),
                      colnames(populationByRace2011),
                      colnames(perCapitaIncome2011),
                      colnames(povertyStatusByAge2013),
                      colnames(employmentStatus2011))
oldEstimateColumns <- columnNamesFirst[grep("Estimate",columnNamesFirst)]

# 2019 column names
columnNamesRecent <- c(colnames(totalPopulationAlone2019),
                        colnames(populationByRace2019),
                        colnames(perCapitaIncome2019),
                        colnames(povertyStatusByAge2019),
                        colnames(employmentStatus2019))

recentEstimateColumns <- columnNamesRecent[grep("Estimate",columnNamesRecent)]
 #raw data storage of column names
columnNamesKey <- data.frame(oldEstimateColumns,recentEstimateColumns)

codifyProperties <- function(columnNameKeyRow) {
  #print(columnNameKeyRow[c('oldEstimateColumns','recentEstimateColumns')])
  my.property <- readline(prompt = paste("property clause: "))
  return(my.property)
}
addAnyPropertyTags <- function(columnPropertyKeyRow) {
  #print(columnPropertyKeyRow[c('oldEstimateColumns','recentEstimateColumns',
  #                         'columnNameKeyCodes')])
  my.property <- readline(prompt = paste("extra tags: "))
  return(my.property)
}
addUnits <- function(columnNameKeyRow) {
  #print(columnNameKeyRow[c('oldEstimateColumns','recentEstimateColumns')])
  my.property <- readline(prompt = paste("specified units: "))
  return(my.property)
}


columnNamesKeyCodes <- apply(columnNamesKey, MARGIN = 1, codifyProperties)

columnPropertyKey <- cbind(columnNamesKey,columnNamesKeyCodes)

columnTags <- apply(columnPropertyKey, MARGIN = 1, addAnyPropertyTags)

unitTags <- apply(columnNamesKey, MARGIN = 1, addUnits)
columnPropertyKeyWithTags <- cbind(columnPropertyKey, columnTags, unitTags)
columnPropertyKeyWithTags$columnTags[columnPropertyKeyWithTags$columnTags ==
                                       ""] <- NA
columnPropertyKeyWithTags$unitTags[columnPropertyKeyWithTags$unitTags ==
                                       ""] <- NA
columnPropertyKeyWithTags <<- columnPropertyKeyWithTags

getPropertyFromCode <- function(columnLabel, columnPropertyKeyWithTags) {
  #work the column label down to just the variable code
  columnVarCode <- tail(unlist(str_split(columnLabel, "/")),1)
  #Select the row that contains a match for columnLabel
  # return(unname(filter(columnPropertyKeyWithTags,
  #               str_detect(oldEstimateColumns, "B02001_003E"))['columnNamesKeyCodes']))
  return(filter(columnPropertyKeyWithTags,
                str_detect(oldEstimateColumns, columnVarCode)))
}

# # specifierSorter <- function(dataSet) {
# #   
# #   columnParser <- function(dataSetColumnName) {
# #     #"Margin of Error!!Total!! XXX Some other race alone XXX /B02001_007M" 
# #   }
# # }
# 
# 
# 
# #  FINDING UNIQUE DESCRIPTIONS  #
# # process for creating unique descriptors, present below in the function
# # names <- colnames(testSetColfax, do.NULL = TRUE)
# # str_split(names[89], "!!", n = Inf, simplify = FALSE)
# # words <- lapply(names, str_split, "!!", n = Inf, simplify = FALSE)
# # words2 <- unlist(words, recursive = TRUE)
# # words3 <- unique(words2, incomparables = FALSE)
# 
# #makes you a list of "unique descriptors", specifically perataining to the
# #   ACS/ Census variable format style: "!!" splits.
# uniqueDescriptors <- function(dataSet) {
#   names <- colnames(dataSet, do.NULL = TRUE) %>%
#     lapply(str_split, "/", n = Inf, simplify = FALSE) %>%
#     lapply(str_split, "!!", n = Inf, simplify = FALSE) %>%
#     unlist(recursive = TRUE) %>%
#     unique(incomprables = FALSE)
#   return(names)
# }
# uniqueDescriptors2EclecticBoogaboo <- function(dataSet) {
#   names <- colnames(dataSet, do.NULL = TRUE) %>%
#     lapply(str_split, "/") %>%
#     lapply(unlist) %>%
#     lapply(head, 1) %>%
#     lapply(str_split, "!!", n = Inf, simplify = FALSE) %>%
#     unlist(recursive = TRUE) %>%
#     unique(incomparables = FALSE)
#   return(names)
# }
# 
# # considerations for using uniqueDescriptors to better assign the True Numbers 
# #     properties, at least by providing more guidance to the user.  
# uniqueDescriptors(financeData)
# 


# OLD FRAMEWORK FOR EARLY POTENTIAL SOLUTION #
# columnToTrueNumbers <- function(dataSet, columnIndexToIngest) {
#   #Property, consistent throughout column
#   setPropertyAndTags()
#   #Subject, changes per instance to match tract, county, etc.
#   
# #Tags, ambiguous, a real pain, likely consistent with property though, I hope
# }

## OLD CODE I COULDNT BE SURE I DIDNT NEED YET ##
# some examples and tests mostly #
# unspecified generality, just grabs general info about our df.
# tags = c(yATKey, groupVariablesInAList) #also see far right column first row

# subject = paste("new_mexico",
#                 paste("county", "santa_fe", sep = ":"),
#                 paste("tract", "000200", sep = ":"), sep = "/")
# #"new_mexico/county:santa_fe/tract:000200"

# Builds a single tnum subject, using the colon and slash functions
# subject2 = slashSep(c("new_mexico",
#                      colonSep(c("county","santa_fe")),
#                      colonSep(c("tract", "000200"))))

# property = paste(paste("some_other_race", "two_plus_races", sep = ":"),
#                  paste("population", "estimated", sep = ":"),
#                  sep = "/")

# Builds a single tnum property, using the colon and slash functions
# property2 = slashSep(c(colonSep(c("some_other_race", "two_plus_races")),
#                        colonSep(c("population", "estimated"))))

#Next up, we try to make a function that will read a column of values and make
# true numbers for each value, In other words with a set property, 
# by reading in the value, 
# and parsing its associated geo's name into a subject. We get true numbers!!!!

#pretty slick...

# Going for Column 7 of the testSFGroupB02001 set
#Set the property:
# theProperty = slashSep(c(colonSep(c("some_other_race", "and",
#                        "two_plus_races")),
#                        colonSep(c("population", "estimated"))))

# Best I can tell, these are just some example assignments for function building
# #Read in the (NAME, NUMBER) tuple from the data set
# # EX VALUE # 17 #
# testSFGroupB02001[41,7]
# 
# theReference = testSFGroupB02001[, c(1:4,7)]
# 
# theValues = testSFGroupB02001[,7]

# Builds a list containing a subject phrase, and a corresponding value.
# setTheSubjectValue(testSFGroupB02001[2,])
# setTheSubjectValue <- function(valueNameReference) {
#   countyTitle <- generateCountyString(valueNameReference['NAME'])
#   if (is.na(valueNameReference['tract'])) {
#     if (is.na(valueNameReference['county'])) {
#       if (is.na(valueNameReference['state'])) {
#         subject <- ("united_states")
#       }
#       subject <- ("new_mexico")
#     }
#     subject <- (slashSep(c("new_mexico",
#                       colonSep(c("county", countyTitle)))))
#   }
#   subject <- (slashSep(c("new_mexico",
#                     colonSep(c("county", countyTitle)),
#                     colonSep(c("tract", valueNameReference['tract'])))))
#   return(c(subject, valueNameReference[7]))  #HARD
# }

# apply(testSFGroupB02001, MARGIN = 1, setTheSubject)
#this call makes a big old list of subject values for TrueNumbers

# if (last(valueNameReference) == "1") {
#   return "united_states"
# } else if (isNA.valueNameReference[2]) {
#   return "new_mexico"
# }
# 
# subject = slashSep(c(valueNameReference[],
#                         colonSep(c("county","santa_fe")),
#                         colonSep(c("tract", "000200"))))

#use apply
# 
# str_split(testSFGroupB02001[4,4], ", ")
# 
# 
# nameList <- unlist(str_split(valueNameReference['NAME'], ", "))
# countyName <- nameList[grepl("County", nameList)]


