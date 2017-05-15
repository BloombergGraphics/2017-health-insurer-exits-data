# Rhode Island data

library(dplyr)
library(tidyr)

source("scripts/commonFunctions.R")

# HIOS ids for matching, from makeHiosIds.R
ids_ri <- getHiosIds("RI")

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_ri <- fips_codes %>% filter(state_code == "RI") %>%
	select(fips_county, county_name, state_code)

####################################################################################
# Marketplace plans
# 2017 Plans from http://healthsourceri.com/wp-content/uploads/2016/12/2017Individual-Plan-Rates-by-age_11102016-1.pdf
# 2014-2016 insurers from http://healthsourceri.com/wp-content/uploads/2016/07/OE3Report_Final_07212016.pdf
####################################################################################

ri17 <- data.frame(c("Blue Cross & Blue Shield of Rhode Island", "Neighborhood Health Plan of Rhode Island"), c(11, 6))
colnames(ri17) <- c("issuer_name", "plans")

ri16 <- data.frame(c("Blue Cross & Blue Shield of Rhode Island", "Neighborhood Health Plan of Rhode Island", "United HealthCare of New England, Inc."))
colnames(ri16) <- "issuer_name"
# 2015 insurers same as 2016
ri15 <- ri16

ri14 <- data.frame(c("Blue Cross & Blue Shield of Rhode Island", "Neighborhood Health Plan of Rhode Island"))
colnames(ri14) <- "issuer_name"

# Add years
ri17 <- ri17 %>% mutate(year = 2017)
ri16 <- ri16 %>% mutate(year = 2016)
ri15 <- ri15 %>% mutate(year = 2015)
ri14 <- ri14 %>% mutate(year = 2014)

ri <- bind_rows(ri14, ri15, ri16, ri17)

# Add fips etc
ri <- ri %>% mutate(state_code = "RI", 
										rating_area = 1)

# Add HIOS id
ri <- left_join(ri, ids_ri, by="issuer_name")

# Single rating area - get county fips and names

ri <- left_join(fips_ri, ri, by = "state_code")
ri <- ri %>% select(year, fips_county, county_name, state_code, issuer_name, issuer_id, everything()) %>%
	arrange(year, fips_county)

write.csv(ri, "data-original/state-based/ri-insurers.csv", row.names = F, na = "")

####################################################################################
# County enrollment
# 2017 data provided via email on 04/0717 - through 01/31/17
####################################################################################
ri_enroll17 <- read.csv("documents/ri/2017-ri-county-enrollment.csv", stringsAsFactors = F)

# Add fips etc
ri_enroll17 <- ri_enroll17 %>% mutate(county_name = paste(county_name, "County", sep=" "),
																			year = 2017)
ri_enroll17 <- left_join(ri_enroll17, fips_ri, by = "county_name") %>%
	select(year, fips_county, everything())
write.csv(ri_enroll17, "data-original/state-based/ri-county-enrollment.csv", row.names = F, na = "")
