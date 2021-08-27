#title: ACS 5 year survey retrieval for NCNM
#author: Aidan O'Hara
#date: 8/25/2021


#libraries
library(dplyr)
library(bea.R)

#BEA credentials
beaKey 	<- 'YOUR 36-DIGIT API KEY'


#List of BEA tables to focus

#beaSEARCH function

#functions for county, state, and national geos

#beaGET function: needs TableID

#example data retrieval from bea.R readme
#beaSpecs <- list(
#  'UserID' = beaKey ,
#  'Method' = 'GetData',
#  'datasetname' = 'NIPA',
#  'TableName' = 'T20305',
#  'Frequency' = 'Q',
#  'Year' = 'X',
#  'ResultFormat' = 'json'
#);
#beaPayload <- beaGet(beaSpecs);


#Df manipulation

#added fun note beaViz is specifically for creating a visual dashboard!