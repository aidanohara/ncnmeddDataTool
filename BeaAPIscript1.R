#title: ACS 5 year survey retrieval for NCNM
#author: Aidan O'Hara
#date: 8/25/2021


#libraries
library(dplyr)
library(bea.R)

#BEA credentials
beaKey 	<- ''


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
#  'Year' = '2011',
#  'ResultFormat' = 'json'
#);
#beaPayload <- beaGet(beaSpecs);

# BEA seems to be in the process of dismantling 'Regional' into 
#  two subsets, the API hasn't recieved the update yet.  


beaSpecs <- list(
  'UserID' = beaKey,
  'Method' = 'GetData',
  'datasetname' = 'Regional', #Regional data has been split up by BEA
  'TableName' = 'CA1',
  'LineCode' = '2',
  'Frequency' = 'Q',
  'Year' = '2011',
  'GeoFips' = '35049',
  'ResultFormat' = 'json'
);
beaPayload <- beaGet(beaSpecs);
  
)

#Df manipulation

#added fun note beaViz is specifically for creating a visual dashboard!