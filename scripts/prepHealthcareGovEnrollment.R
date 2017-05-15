# Get and format healthcare.gov county-level enrollment

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(RSocrata)

####################################################################################
# 2015 and 2016 from data.cms.gov
# Files also saved as .csv on 04-06-17 for future use if needed
####################################################################################
formatSocrata <- function(dataurl, year) {
	dt <- read.socrata(dataurl)
	colnames(dt) <- tolower(str_replace_all(colnames(dt), "[.]", "_"))
	dt <- as.data.frame(dt)
	dt[dt == "."] <- NA
	dt[,-c(1:3)] <- sapply( dt[,-c(1:3)], as.numeric )
	dt <- dt %>% mutate(year = year)
	return(dt)
}

aptc15 <- formatSocrata("https://data.cms.gov/Marketplace-Qualified-Health-Plan-QHP-/2015-Qualifying-Health-Plan-Selections-by-APTC-and/ir53-huib", 2015)
aptc16 <- formatSocrata("https://data.cms.gov/Marketplace-Qualified-Health-Plan-QHP-/2016-Qualifying-Health-Plan-Selections-by-APTC-and/68bp-xniz", 2016)
csr15 <- formatSocrata("https://data.cms.gov/Marketplace-Qualified-Health-Plan-QHP-/2015-Qualifying-Health-Plan-Selections-by-CSR-and-/rj5u-mfix", 2015)
csr16 <- formatSocrata("https://data.cms.gov/Marketplace-Qualified-Health-Plan-QHP-/2016-Qualifying-Health-Plan-Selections-by-CSR-and-/8q7u-fe6a", 2016)

enroll15 <- left_join(aptc15, csr15, by = c("county_fips_code", "county_name", "state", "year"))
# Just making sure no difference
enroll15 <- enroll15 %>% mutate(temp = total_plan_selections.x - total_plan_selections.y)
# Good no difference, so delete
summary(enroll15$temp)
enroll15 <- enroll15 %>% select(-total_plan_selections.y, -temp) %>%
	rename(state_code = state)

enroll16 <- left_join(aptc16, csr16, by = c("county_fips_code", "county_name", "state_name", "year"))
# Just making sure no difference
enroll16 <- enroll16 %>% mutate(temp = total_plan_selections.x - total_plan_selections.y)
# Good no difference, so delete
summary(enroll16$temp) 
enroll16 <- enroll16 %>% select(-total_plan_selections.y, -temp) %>%
	rename(state_code = state_name)

enroll <- rbind(enroll15, enroll16)
enroll <- enroll %>% rename(fips_county = county_fips_code, 
														plan_selections = total_plan_selections.x,
														plan_selections_aptc = yes_aptc,
														plan_selections_noaptc = no_aptc,
														plan_selections_csr = yes_csr,
														plan_selections_nocsr = no_csr) %>%
	mutate(fips_county = sprintf("%05s", fips_county)) %>%
	select(year, fips_county, county_name, state_code, plan_selections, everything()) %>%
	filter(fips_county != "00000" & fips_county != "XXXXX")
	
####################################################################################
# Health Insurance Marketplaces 2017 Open Enrollment Period: Final County-Level Public Use File
# 2017 Open Enrollment Period - Final Open Enrollment Report
# Nov. 1, 2016 — Jan. 31, 2017
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Marketplace-Products/Plan_Selection_ZIP.html

# Source notes:
# * Data are for the 39 states that use the HealthCare.gov enrollment platform for 2017.
# ** Counties with 50 or fewer plan selections are not included.
####################################################################################

#download.file("https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Marketplace-Products/Downloads/OE2017_COUNTY_PUF_FINAL.zip", "data-original/enrollment/OE2017_COUNTY_PUF_FINAL.zip")
#unzip("data-original/enrollment/OE2017_COUNTY_PUF_FINAL.zip", exdir = "data-original/enrollment/")

enroll17 <- readWorkbook("data-original/enrollment/OE2017_COUNTY_PUF_FINAL.xlsx", sheet = "County PUF", startRow = 23, colNames = TRUE, na.strings = c("", "*", "-"))
# Match colnames to plan data
colnames(enroll17) <- gsub('[.]', '_',tolower(colnames(enroll17)))
enroll17 <- enroll17 %>% rename(fips_county = county_fips_code, state_code = state, county_name = county, plan_selections_aptc = consumers_with_aptc)
enroll17 <- enroll17  %>% mutate(year = 2017)

write.csv(enroll17, "data/2017-healthcaregov-county-enrollment.csv", row.names = F, na = "")

# Save all years joined
enroll <- bind_rows(enroll, enroll17) %>%
	arrange(year, fips_county)
write.csv(enroll, "data/healthcaregov-county-enrollment.csv", row.names = F, na = "")
