# Colorado insurer data 

library(dplyr)
library(tidyr)
library(openxlsx)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_co <- fips_codes %>% filter(state_code == "CO") %>%
	select(fips_county, county_name)

####################################################################################
# 2017 Insurers from https://www.colorado.gov/dora/node/100241
# Assembled PDF into CSV with tabula and by hand
# Convert wide data to long to match FFM data
# 2016 data from SBM PUFs provided by CMS
# 2015, 2014 from SERFF filings
####################################################################################
co_raw <- read.csv("data-original/state-based/raw/colorado-insurers-raw-2017.csv", stringsAsFactors = F)

co17 <- co_raw %>% gather(issuer_name, plans, -c(county, Total, rating_area)) %>% select(-Total) %>% 
	separate(county, c("county_name", "fips_county"), sep = " - ") %>%
	mutate(state_code = "CO", year = 2017) %>%
	select(year, fips_county, state_code, county_name, everything()) %>%
	filter(!is.na(plans)) %>%
	arrange(fips_county)

co17$issuer_name <- str_replace_all(co17$issuer_name, "[.]", " ")
write.csv(co17, "data-original/state-based/2017-co-insurers.csv", row.names = F, na = "")

####################################################################################
# Enrollment by county
# 2016 and 2017 spreadsheet provided on 04/05/17 by email request
####################################################################################

# Join data from separate worksheets
e1 <- readWorkbook("documents/co/Connect for Health Colorado_County_Level 2017.xlsx", sheet = "Enrollment by County", startRow = 2)
e2 <- readWorkbook("documents/co/Connect for Health Colorado_County_Level 2017.xlsx", sheet = "Tax Credit by County", startRow = 2)
colnames(e1)
colnames(e2)
# Same names as healthcare.gov
e1 <- e1 %>% rename(county_name = `County.Name:`) %>%
	select(-`%.Change`) %>%
	gather(year,  plan_selections, -county_name)
e2 <- e2 %>% rename(county_name = `County.Name:`) %>%
	select(-`%.increase`) %>%
	gather(year, average_aptc_for_consumers_with_aptc, -county_name)

co_enroll <- left_join(e1, e2, by = c("year", "county_name"))
# fips
co_enroll <- co_enroll %>% filter(county_name != "BILLING ADDRESS NOT IN COLORADO") %>%
	rename(name_match = county_name)
fips_co <- fips_co %>% mutate(name_match = toupper(str_replace_all(county_name, " County", "")))
co_enroll <- left_join(co_enroll, fips_co, by = "name_match")

co_enroll <- co_enroll %>% select(-name_match) %>%
	mutate(state_code = "CO") %>%
	select(year, fips_county, county_name, state_code, everything())
write.csv(co_enroll, "data-original/state-based/co-county-enrollment.csv", row.names = F, na = "")
