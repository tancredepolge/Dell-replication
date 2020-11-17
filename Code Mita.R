library(tidyverse)
library(rio)
library(lmtest)
library(sandwich)
library(stargazer)

mitaData <- import('./mitaData.dta')
#View(mitaData)

    #### EXERCISE 1 ####

    ## Question C ##

# Constructing the longitude and latitude polynomial terms
mitaData$lon2 <- mitaData$lon^2
mitaData$lat2 <- mitaData$lat^2
mitaData$lonlat <- mitaData$lon * mitaData$lat
mitaData$lon3 <- mitaData$lon^3
mitaData$lat3 <- mitaData$lat^3
mitaData$lon2lat <- mitaData$lon^2 * mitaData$lat
mitaData$lonlat2 <- mitaData$lon * mitaData$lat^2

#  Constructing samples according to distances to mita boundaries
mitaData_100 <- filter(mitaData, d_bnd < 100)
mitaData_75 <- filter(mitaData, d_bnd < 75)
mitaData_50 <- filter(mitaData, d_bnd < 50)

# Regression with distance to boundary < 100 km 
reg_1a <- lm(lhhequiv ~ pothuan_mita 
             + lon + lat + lon2 + lat2 + lonlat + lon3 + lat3 + lon2lat + lonlat2
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_100)
reg_1a_cse <- coeftest(reg_1a, vcovCL, cluster = mitaData_100$district)[,2]

# Regression with distance to boundary < 75 km 
reg_1b <- lm(lhhequiv ~ pothuan_mita 
             + lon + lat + lon2 + lat2 + lonlat + lon3 + lat3 + lon2lat + lonlat2
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_75)
reg_1b_cse <- coeftest(reg_1b,vcovCL, cluster = mitaData_75$district)[,2]

# Regression with distance to boundary < 50 km 
reg_1c <- lm(lhhequiv ~ pothuan_mita 
             + lon + lat + lon2 + lat2 + lonlat + lon3 + lat3 + lon2lat + lonlat2
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_50)
reg_1c_cse <- coeftest(reg_1c,vcovCL, cluster = mitaData_50$district)[,2]

stargazer(reg_1a, reg_1b, reg_1c, se = list(reg_1a_cse, reg_1b_cse, reg_1c_cse), keep = "pothuan_mita", type = "text")
#stargazer(reg_1a, reg_1b, reg_1c, se = list(reg_1a_cse, reg_1b_cse, reg_1c_cse), keep = "pothuan_mita", type = "latex", out ="/Users/tancredepolge/Documents/M2/M2S1/Political & Organizational/Replication/Tex/reg1.tex")

    ## Question D ##

# Constructing the distance to Potosi polynomial terms
mitaData$dpot2 <- mitaData$dpot^2
mitaData$dpot3 <- mitaData$dpot^3

#  Re-constructing samples according to distances to mita boundaries (with the new variables dpot^2 and dpot^3)
mitaData_100 <- filter(mitaData, d_bnd < 100)
mitaData_75 <- filter(mitaData, d_bnd < 75)
mitaData_50 <- filter(mitaData, d_bnd < 50)

# Regression with distance to boundary < 100 km 
reg_2a <- lm(lhhequiv ~ pothuan_mita 
             + dpot + dpot2 + dpot3 
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_100)
reg_2a_cse <- coeftest(reg_2a,vcovCL, cluster = mitaData_100$district)[,2]

# Regression with distance to boundary < 75 km 
reg_2b <- lm(lhhequiv ~ pothuan_mita 
             + dpot + dpot2 + dpot3 
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_75)
reg_2b_cse <- coeftest(reg_2b,vcovCL, cluster = mitaData_75$district)[,2]

# Regression with distance to boundary < 50 km 
reg_2c <- lm(lhhequiv ~ pothuan_mita 
             + dpot + dpot2 + dpot3 
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_50)
reg_2c_cse <- coeftest(reg_2c,vcovCL, cluster = mitaData_50$district)[,2]

stargazer(reg_2a, reg_2b, reg_2c, se = list(reg_2a_cse, reg_2b_cse, reg_2c_cse), keep = "pothuan_mita",type = "text") 
#stargazer(reg_2a, reg_2b, reg_2c, se = list(reg_2a_cse, reg_2b_cse, reg_2c_cse), keep = "pothuan_mita", type = "latex", out ="/Users/tancredepolge/Documents/M2/M2S1/Political & Organizational/Replication/Tex/reg2.tex")

    ## Question F ##

mitaData_short <- aggregate(mitaData, by = list(mitaData$district), FUN = "mean")
#View(mitaData_short)

# Constructing the distance to Potosi polynomial terms
mitaData_short$dpot2 <- mitaData_short$dpot^2
mitaData_short$dpot3 <- mitaData_short$dpot^3

#  Re-constructing samples according to distances to mita boundaries (with the new variables dpot^2 and dpot^3)
mitaData_short_100 <- filter(mitaData_short, d_bnd < 100)
mitaData_short_75 <- filter(mitaData_short, d_bnd < 75)
mitaData_short_50 <- filter(mitaData_short, d_bnd < 50)

# Regression with distance to boundary < 100 km 
reg_3a <- lm(lhhequiv ~ pothuan_mita 
             + dpot + dpot2 + dpot3 
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_short_100)

# Regression with distance to boundary < 75 km 
reg_3b <- lm(lhhequiv ~ pothuan_mita 
             + dpot + dpot2 + dpot3 
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_short_75)

# Regression with distance to boundary < 50 km 
reg_3c <- lm(lhhequiv ~ pothuan_mita 
             + dpot + dpot2 + dpot3 
             + elv_sh + slope + infants + children + adults + bfe4_1 + bfe4_2 + bfe4_3, 
             data = mitaData_short_50)

stargazer(reg_3a, reg_3b, reg_3c, keep = "pothuan_mita",type = "text")
#stargazer(reg_3a, reg_3b, reg_3c, keep = "pothuan_mita", type = "latex", out ="/Users/tancredepolge/Documents/M2/M2S1/Political & Organizational/Replication/Tex/reg3.tex")
