library(tidyverse)
library(rio)
library(miceadds) ## Contains the lm.cluster function. Have to install the package 'mice' as well. 
library(jtools)

mitaData <- import('./mitaData.dta')
View(mitaData)


    #### EXERCISE 1 ####

    ## Question C ##

# Constructing the longitude and latitude polynomial terms
mitaData$'lon^2' <- mitaData$lon^2
mitaData$'lat^2' <- mitaData$lat^2
mitaData$'lon*lat' <- mitaData$lon * mitaData$lat
mitaData$'lon^3' <- mitaData$lon^3
mitaData$'lat^3' <- mitaData$lat^3
mitaData$'lon^2*lat' <- mitaData$lon^2 * mitaData$lat
mitaData$'lon*lat^2' <- mitaData$lon * mitaData$lat^2

#  Constructing samples according to distances to mita boundaries
mitaData_100 <- filter(mitaData, d_bnd < 100)
mitaData_75 <- filter(mitaData, d_bnd < 75)
mitaData_50 <- filter(mitaData, d_bnd < 50)

# Regression with distance to boundary < 100 km 
reg_1a <- lm.cluster(data = mitaData_100, lhhequiv ~ pothuan_mita + mitaData_100$'lon^2' + mitaData_100$'lat^2' + mitaData_100$'lon*lat' + mitaData_100$'lon^3' + mitaData_100$'lat^3' + mitaData_100$'lon^2*lat' + mitaData_100$'lon*lat^2' + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, cluster = mitaData_100$district)
summary(reg_1a)
lm.reg_1a <- lm(lhhequiv ~ pothuan_mita + mitaData_100$'lon^2' + mitaData_100$'lat^2' + mitaData_100$'lon*lat' + mitaData_100$'lon^3' + mitaData_100$'lat^3' + mitaData_100$'lon^2*lat' + mitaData_100$'lon*lat^2' + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data = mitaData_100)
summary(lm.reg_1a)
jtools::summ(lm.reg_1a, cluster = "mitaData_100$district")


# Regression with distance to boundary < 75 km 
reg_1b <- lm.cluster(data = mitaData_75, lhhequiv ~ pothuan_mita + mitaData_75$'lon^2' + mitaData_75$'lat^2' + mitaData_75$'lon*lat' + mitaData_75$'lon^3' + mitaData_75$'lat^3' + mitaData_75$'lon^2*lat' + mitaData_75$'lon*lat^2' + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, cluster = mitaData_75$district )
summary(reg_1b)

# Regression with distance to boundary < 50 km 
reg_1c <- lm.cluster(data = mitaData_50, lhhequiv ~ pothuan_mita + mitaData_50$'lon^2' + mitaData_50$'lat^2' + mitaData_50$'lon*lat' + mitaData_50$'lon^3' + mitaData_50$'lat^3' + mitaData_50$'lon^2*lat' + mitaData_50$'lon*lat^2' + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, cluster = mitaData_50$district)
summary(reg_1c)


    ## Question D ##

# Constructing the distance to Potosi polynomial terms
mitaData$'dpot^2' <- mitaData$dpot^2
mitaData$'dpot^3' <- mitaData$dpot^3

#  Re-constructing samples according to distances to mita boundaries (with the new variables dpot^2 and dpot^3)
mitaData_100 <- filter(mitaData, d_bnd < 100)
mitaData_75 <- filter(mitaData, d_bnd < 75)
mitaData_50 <- filter(mitaData, d_bnd < 50)

# Regression with distance to boundary < 100 km 
reg_2a1 <- lm.cluster(data = mitaData_100, lhhequiv ~ pothuan_mita + mitaData_100$dpot + mitaData_100$'dpot^2'+ mitaData_100$'dpot^3' + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, cluster = mitaData_100$district)
summary(reg_2a1)
reg_2a2 <- lm(lhhequiv ~ pothuan_mita + mitaData_100$dpot + mitaData_100$'dpot^2'+ mitaData_100$'dpot^3' + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, data = mitaData_100)
summary(reg_2a2)
summary(reg_2a2, cluster = "mitaData_100$district")

# Regression with distance to boundary < 75 km 
reg_2b <- lm.cluster(data = mitaData_75, lhhequiv ~ pothuan_mita + mitaData_75$dpot + mitaData_75$'dpot^2'+ mitaData_75$'dpot^3' + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, cluster = mitaData_75$district)
summary(reg_2b)

# Regression with distance to boundary < 50 km 
reg_2c <- lm.cluster(data = mitaData_50, lhhequiv ~ pothuan_mita + mitaData_50$dpot + mitaData_50$'dpot^2'+ mitaData_50$'dpot^3' + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, cluster = mitaData_50$district)
summary(reg_2c)

# Test change

