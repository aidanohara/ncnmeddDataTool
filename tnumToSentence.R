#title: True Numbers to Sentences
#author: Aidan O'Hara
#date: 10/26/2021


library(stringr)
library(tnum)
### True numbers to sentences

# subject #
#"new_mexico/county:colfax/household:family"
# family household(s) of colfax county of new mexico

#"new_mexico/county:colfax/tract:950600"
# 950600 census tract of colfax county of new mexico

# county:colfax/ -> "of colfax county"
# new_mexico/ -> "of New Mexico"
# tract:950600 -> "950600 census tract"

# property #
#"population:estimated"
# estimated population
#"per_capita:income:estimated"
# estimated income per capita

#"new_mexico/county:colfax/tract:950600"
#"new_mexico" "county:colfax" "tract:950600"
#"tract:950600" "county:colfax" "new_mexico"
#"tract:950600" "county:colfax" "new mexico"
### "tract:950600"
### "tract" "950600"
### "950600 census tract"
#"950600 census tract" "colfax county" "new mexico"
#"950600 census tract of colfax county of new mexico"

#simplest version#
#function #input: some path string
# output, path string as white-space sentence w/reverse order
pathToWSSen <- function(pathString) {
  pathList <- strsplit(pathString, split = "/")
  noUnderscores <- lapply(pathList ,stringr::str_replace, pattern = "_",
                                                    replacement = " ")
  noColon <- lapply(unlist(noUnderscores), function(adjPhrase) {
    adjList <- strsplit(adjPhrase, split = ":")
    wordsList <- rev(unlist(adjList))
    phrase <- paste(wordsList, collapse = " ")
    }) 
  return(paste(unlist(rev(noColon)), collapse = " of "))
}

# tnum ->
# asian race of colfax county of new mexico has estimated population = 74 +/- 18

#function #input: a single tnum, or a list of tnums.
# ouput: a list with sentence-versions of the tnums. 
tnumToSentence <- function(someTrueNumber) {
  # List of Tnums/One Tnum -> to dataframe
  aBox <- tnum.objectsToDf(someTrueNumber)
  # By row of the dataframe, 
  apply(aBox, MARGIN = 1, function(aTnumRow) {
    ### make WS phrases from subject and property
    subjectWS <- pathToWSSen(aTnumRow['subject'])
    propertyWS <- pathToWSSen(aTnumRow['property'])
    tnumError <- as.double(aTnumRow['error'])
    tnumValue <- aTnumRow['numeric.value']
    ### construct "= x +/- y" string from value and error.
    ##### if error is <= 0 construct "= x" from value
    if ((tnumError < 0) || (tnumError == 0) || 
        (is.na(tnumError))) {
      valuePhrase <- paste(" = ", tnumValue)
    } else {
      valueError <- paste(tnumValue, "+/-", tnumError)
      valuePhrase <- paste(" = ", valueError, sep = "")
    }
    ### paste subject property with "has"
    subjectProperty <- paste(subjectWS, propertyWS, sep = " has ")
    ### paste sub/prop and val/error
    tnumSentence <- paste(subjectProperty, valuePhrase, sep = "")
    
    # add a year footnote #
    splitTags <- str_split(aTnumRow['tags'], ",")
    #print(splitTag)
    tnumYear <- splitTags[[1]][grep("year", splitTags[[1]])]
    tnumYearAlone <- str_split(tnumYear, ":")
    tnumYearFinal <- tnumYearAlone[[1]][!grepl("year", tnumYearAlone[[1]])]
    
    tnumSentenceWithYear <- paste(tnumSentence, tnumYearFinal, sep = "; ")
    return(paste(tnumSentenceWithYear, ".", sep = ""))
  })
}

