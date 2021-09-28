#True Numbers intake test space
# HEADER HERE

library(tnum)
library(stringr)

source("CensusAPIscript1.R")

#old census data retriever call
#acs5Data2019 <- aCS5DataRetriever(2019)

#select just the first row, and first 5 columns...
#populationTotalTest <- acs5Data2019[2,(1:5)]

# evenFirsterTnum <- tnum.makeObject(
#   subject = "2019/new_mexico/county:santa_fe/tract:12-05",
#   property = "population:estimated:total",
#   value = 5719
# )

#generalized subject
# year/stateName/county:countyName/tract:tractNumber

# firstTnum <- tnum.makeObject(
#   subject = "university:state:montclair",
#   property = "rate:graduation",
#   value = populationTotalTest[,5],
#   unit = "%"
# )

#modern Census data retrieval call, just used for tests though
#testSetColfax <- acsDataRetriever(2019, 5, "state:35", 
#                                 "007", groupsForVariables)


# process for creating unique descriptors, present below in the function
# names <- colnames(testSetColfax, do.NULL = TRUE)
# str_split(names[89], "!!", n = Inf, simplify = FALSE)
# words <- lapply(names, str_split, "!!", n = Inf, simplify = FALSE)
# words2 <- unlist(words, recursive = TRUE)
# words3 <- unique(words2, incomparables = FALSE)

#makes you a list of "unique descriptors", specifically perataining to the
#   ACS/ Census variable format style: "!!" splits.
uniqueDescriptors <- function(dataSet) {
  names <- colnames(dataSet, do.NULL = TRUE) %>%
    lapply(str_split, "!!", n = Inf, simplify = FALSE) %>%
    unlist(recursive = TRUE) %>%
    unique(incomprables = FALSE)
  return(names)
}

# another modern census retriever, specifically just has race data. 
#    used for building first tnums
testSFGroupB02001 <- acsDataRetriever(2019, 5, "state:35",
                                      "049", "B02001")

# format for column 7 in the df.
# santaFeTnum <- tnum.makeObject(
#   subject = "new_mexico/county:santa_fe/tract:000200",
#   property = "some_other_race:two_plus_races/population:estimated",
#   value = 17,
#   tags = c("2019/acs/acs5", "group_B02001")
# )

# random variable for value
#value = 17

# unspecified generality, just grabs general info about our df.
tags = c(yATKey, groupVariablesInAList) #also see far right column first row


# Function that places ":" between input strings
colonSep <- function(stringsToCombine) {
  paste(stringsToCombine, collapse = ":")
}

# Function that places "/" between input strings
slashSep <- function(stringsToCombine) {
  paste(stringsToCombine, collapse = "/")
}

# subject = paste("new_mexico",
#                 paste("county", "santa_fe", sep = ":"),
#                 paste("tract", "000200", sep = ":"), sep = "/")
# #"new_mexico/county:santa_fe/tract:000200"

# Builds a single tnum subject, using the colon and slash functions
subject2 = slashSep(c("new_mexico",
                     colonSep(c("county","santa_fe")),
                     colonSep(c("tract", "000200"))))

# property = paste(paste("some_other_race", "two_plus_races", sep = ":"),
#                  paste("population", "estimated", sep = ":"),
#                  sep = "/")

# Builds a single tnum property, using the colon and slash functions
property2 = slashSep(c(colonSep(c("some_other_race", "two_plus_races")),
                       colonSep(c("population", "estimated"))))

#Next up, we try to make a function that will read a column of values and make
# true numbers for each value, In other words with a set property, 
# by reading in the value, 
# and parsing its associated geo's name into a subject. We get true numbers!!!!

#pretty slick...

# Going for Column 7 of the testSFGroupB02001 set
#Set the property:
theProperty = slashSep(c(colonSep(c("some_other_race", "and", "two_plus_races")),
                       colonSep(c("population", "estimated"))))

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
setTheSubjectValue <- function(valueNameReference) {
  countyTitle <- generateCountyString(valueNameReference['NAME'])
  if (is.na(valueNameReference['tract'])) {
    if (is.na(valueNameReference['county'])) {
      if (is.na(valueNameReference['state'])) {
        subject <- ("united_states")
      }
      subject <- ("new_mexico")
    }
    subject <- (slashSep(c("new_mexico",
                      colonSep(c("county", countyTitle)))))
  }
  subject <- (slashSep(c("new_mexico",
                    colonSep(c("county", countyTitle)),
                    colonSep(c("tract", valueNameReference['tract'])))))
  return(c(subject, valueNameReference[7]))  #HARD
}



# apply(testSFGroupB02001, MARGIN = 1, setTheSubject)
#this call makes a big old list of subject values for TrueNumbers

  # 
  # 
  # 
  # 
  # 
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

# "Census Tract 8, Santa Fe County, New Mexico" -> "santa_fe"
# in an effor to avoid building a reference table to go from county-fips to
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


# This oddly simple function always underwhelms and surprises me.  This process,
#   is gonna get a lot more hectic before we make it out of the woods. 
oneTrueNumberPlease <- function(subjectValue, property){
  aTNum <- tnum.makeObject(
    subject = unlist(subjectValue[1]),
    property = property,
    value = unname(unlist(subjectValue[2])),
    tags = c("2019/acs/acs5", "group:B02001") # will generalize later
  )
  return(aTNum)
}

makeTrueNumber <- function(aDataFrameRow, propertyClause, estimateColumnIndex,
                           mOEColumnIndex, listOfTags) {
  #generate a tnum ok county string
  countyTitle <- generateCountyString(aDataFrameRow['NAME'])
  #figure out what geo we're in, build a subject accordingly
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
  #BAD IF TREE< AT LEAST BEST I COULD TELL
  # if (is.na(aDataFrameRow['tract'])) {
  #   if (is.na(aDataFrameRow['county'])) {
  #     if (is.na(aDataFrameRow['state'])) {
  #       subjecter <- ("united_states")
  #     }
  #     subjecter <- ("new_mexico")
  #   }
  #   subjecter <- (slashSep(c("new_mexico",
  #                          colonSep(c("county", countyTitle)))))
  # } else {
  #   subjecter <- (slashSep(c("new_mexico",
  #                            colonSep(c("county", countyTitle)),
  #                            colonSep(c("tract", aDataFrameRow['tract'])))))
  # }
  #set the value
  valueNumber <- aDataFrameRow[estimateColumnIndex]
  #finish building tags
  mOETag <- paste("margin_of_error:",aDataFrameRow[mOEColumnIndex], sep = "")
  listOfTags <- c(listOfTags, mOETag)
  print(subjecter)
  #print(property)
  print(valueNumber)
  #print(listOfTags)
  aNum <- tnum.makeObject(
    subject = subjecter,
    property = propertyClause,
    value = valueNumber,
    tags = listOfTags
  )
  #print(aNum)
  return(aDataFrameRow)
}

#Fold the margin of error data into each true numbers about the data point in Q

userSetProperty <- function(columnNames) {
  print(columnNames)
  print("example: race:some_other:two_plus_races/population:estimated")
  my.property <- readline(prompt = paste("property clause: "))
  return(my.property)
}

extractTaggage <- function(columnStringWithTags) {
  bits <- str_split(columnStringWithTags, ";")
  lessBits <- tail(unlist(bits), n = -1)
  return(lessBits)
}


makePartialsDFs <- function(acsData) {}

makeTNumsFromPartialDF <- function(singleVariableDataFrame) {
  propertyClause <- userSetProperty(colnames(singleVariableDataFrame))
  listOfTags <- extractTaggage(tail(names(singleVariableDataFrame), n = 1))
  estimateColumnIndex <- grep("Estimate", colnames(singleVariableDataFrame))
  mOEColumnIndex <- grep("Margin", colnames(singleVariableDataFrame))
  
  #here the function is gonna deep dive in some kind of "apply"
  theNUMS <- lapply(singleVariableDataFrame, makeTrueNumber, 
        propertyClause, estimateColumnIndex, mOEColumnIndex, listOfTags)
  return(theNUMS)
}





columnToTrueNumbers <- function(dataSet, columnIndexToIngest) {
  #Property, consistent throughout column
  setPropertyAndTags()
  #Subject, changes per instance to match tract, county, etc.
  
  #Tags, ambiguous, a real pain, likely consistent with property though, I hope
}
