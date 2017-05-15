# DC

library(dplyr)
library(tidyr)

source("scripts/commonFunctions.R")

# HIOS ids for matching, from makeHiosIds.R
ids_dc <- getHiosIds("DC")
	
####################################################################################
# Marketplace plans
# 2014 https://disb.dc.gov/sites/default/files/dc/sites/disb/publication/attachments/FinalIndividualRates71913.pdf
# 2015 https://disb.dc.gov/sites/default/files/dc/sites/disb/publication/attachments/9-15-14-Individual-Approved-Rates-for-Health-Insurance-Products-Sold-on-DC-Health-Link.pdf
# 2016 https://disb.dc.gov/sites/default/files/dc/sites/disb/publication/attachments/2016%20QHP%20Rate%20Submission%20Data%20-%20Ind%20as%20of%2010302015.pdf
# 2017 Plans from https://disb.dc.gov/sites/default/files/dc/sites/disb/publication/attachments/2017%20QHP%20Rate%20Submissions%20-%20Plan%20Counts%20%28FINAL%29.pdf
# 2017 https://disb.dc.gov/sites/default/files/dc/sites/disb/publication/attachments/2017%20QHP%20Rate%20Submission%20Data%20-%20Ind%20%28FINAL%29.pdf
####################################################################################

dc14 <- data.frame(c("Aetna Life Insurance Company", "CareFirst BlueChoice  Inc.", "Kaiser Foundation Health Plan of the Mid-Atlantic States, Inc."))
colnames(dc14) <- "issuer_name"

# 2015 issuers same as 2014
dc15 <- dc14

dc16 <- data.frame(c("CareFirst BlueChoice  Inc.", "Kaiser Foundation Health Plan of the Mid-Atlantic States, Inc."))
colnames(dc16) <- "issuer_name"

dc14 <- dc14 %>% mutate(year = 2014)
dc15 <- dc15 %>% mutate(year = 2015)
dc16 <- dc16 %>% mutate(year = 2016)

dc17 <- data.frame(c("CareFirst BlueChoice  Inc.", "Kaiser Foundation Health Plan of the Mid-Atlantic States, Inc."), c(9, 11))
colnames(dc17) <- c("issuer_name", "plans")
dc17 <- dc17 %>% mutate(year = 2017)

dc <- bind_rows(dc14, dc15, dc16, dc17)
# Add fips etc
dc <- dc %>% mutate(fips_county = "11001", 
										state_code = "DC", 
										county_name = "District of Columbia",
										rating_area = 1) %>%
	select(year, fips_county, everything())

# Add HIOS id
dc <- left_join(dc, ids_dc, by="issuer_name")

write.csv(dc, "data-original/state-based/dc-insurers.csv", row.names = F, na = "")

####################################################################################
# Enrollment
# DC is a state and a county...so use federal data
# 2017 source tab 5 https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Marketplace-Products/Plan_Selection_ZIP.html
# 2017 enrollment  21,248, APTC 916 
####################################################################################

dc_enroll <- data.frame(21248, 916)
colnames(dc_enroll) <- c("plan_selections", "plan_selections_aptc")
dc_enroll<- dc_enroll %>% mutate(fips_county = "11001", 
																	state_code = "DC", 
																	county_name = "District of Columbia",
																	year = 2017) %>%
	select(year, fips_county, county_name, state_code, everything())
write.csv(dc_enroll, "data-original/state-based/dc-county-enrollment.csv", row.names = F, na = "")
