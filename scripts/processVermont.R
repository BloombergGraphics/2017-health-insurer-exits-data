# Vermont data
# Single rating area

library(dplyr)
library(tidyr)

source("scripts/commonFunctions.R")
# HIOS ids for matching, from makeHiosIds.R
ids_vt <- getHiosIds("VT")

# get county fips and names
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_vt <- fips_codes %>% filter(state_code == "VT") %>%
	select(fips_county, county_name, state_code)

####################################################################################
# Marketplace plans
# Plans from http://info.healthconnect.vermont.gov/healthplans#PCB
# http://www.dfr.vermont.gov/sites/default/files/Filed%20QHP%20rates.pdf
# http://info.healthconnect.vermont.gov/2016healthplans
# http://info.healthconnect.vermont.gov/2015healthplans
# and https://vt.checkbookhealth.org/
# Two insurers, one rating region - BCBS and MVP
# 2015: 9 plans each
# 2016-2017: 11 plans each
####################################################################################

vt_raw <- data.frame(c("Blue Cross and Blue Shield of Vermont", "MVP Health Plan, Inc."))
colnames(vt_raw) <- "issuer_name"
# Add fips etc
vt14 <- vt_raw %>% mutate(year = 2014,
													plans = NA)
vt15 <- vt_raw %>% mutate(year = 2015,
													plans = 9)
vt16 <- vt_raw %>% mutate(year = 2016,
													plans = 11)
vt17 <- vt_raw %>% mutate(year = 2017,
													plans = 11)
vt <- rbind(vt14, vt15, vt16, vt17)
vt <- as.data.frame(vt)
vt$issuer_name <- as.character(vt$issuer_name)
vt <- vt %>% mutate(state_code = "VT", 
										rating_area = 1)

# Add HIOS id
vt <- left_join(vt, ids_vt, by="issuer_name")
# MVP not matching
vt <- vt %>% mutate(issuer_id = ifelse(issuer_name == "MVP Health Plan, Inc.", 77566, issuer_id))

vt <- left_join(fips_vt, vt, by = "state_code")
vt <- vt %>% select(year, fips_county, everything()) %>%
	arrange(year, fips_county)

write.csv(vt, "data-original/state-based/vt-insurers.csv", row.names = F, na = "")

####################################################################################
# County enrollment
# No response to email so far, use rounded %s from PDF report
# Multipled percentages (from effectuated enrollment!) by state total plan selections
# Percentages add up to 0.99 due to rounding
# http://info.healthconnect.vermont.gov/sites/hcexchange/files/Coverage%20Dashboard-February2017.pdf
####################################################################################

vt_raw17 <- read.csv("documents/vt/2017-vt-county-enrollment-raw.csv", stringsAsFactors = F)
vt_raw17 <- left_join(vt_raw17, fips_vt, by="county_name")
vt_raw17 <- vt_raw17 %>% mutate(year = 2017) %>%
	select(year, fips_county, county_name, state_code, everything())

# Total plan selections from https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Marketplace-Products/Plan_Selection_ZIP.html
vt_stateenroll <- 30682
vt_enroll17 <- vt_raw17 %>% mutate(plan_selections = round(enrollment_share * vt_stateenroll)) %>%
	select(-enrollment_share)

write.csv(vt_enroll17, "data-original/state-based/vt-county-enrollment.csv", row.names = F, na = "")
																