# Maryland data

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_md <- fips_codes %>% filter(state_code == "MD") %>%
	select(fips_county, county_name) %>%
	mutate(county_name = str_replace_all(county_name, "city", "City"))

####################################################################################
# Issuers by county, 2015-2017 provided in spreadsheet via email
####################################################################################
md_raw <- readWorkbook("documents/md/QHP Carriers presence in Counties by year - 04-13-2017.xlsx", sheet = "Dataset")
colnames(md_raw) <- tolower(colnames(md_raw))

# Need to match fips codes, county names are missing spaces
md_raw <- md_raw %>% rename(issuer_name = carrier, name_match = county) %>%
	mutate(name_match = toupper(name_match))

fips_md <- fips_md %>% mutate(name_match = toupper(str_replace_all(county_name, "County", ""))) %>%
	mutate(name_match = str_replace_all(name_match, "ST\\.", "SAINT")) %>%
	mutate(name_match = str_replace_all(name_match, " ", "")) %>%
	mutate(name_match = str_replace_all(name_match, "'", "")) %>%
	mutate(name_match = str_replace_all(name_match, "[.]", ""))

md <- left_join(md_raw, fips_md, by="name_match") %>%
	select(-name_match) %>%
	arrange(year, fips_county) %>%
	mutate(state_code = "MD") %>%
	select(year, fips_county, county_name, everything())
write.csv(md, "data-original/state-based/md-insurers_fromstate.csv", row.names = F, na = "")

####################################################################################
##### PREVIOUS DATA BEFORE EMAILED VERSION SENT FROM MD STATE ##### 
# 2017 Marketplace plans
# Went to MD Health connection, entered a 27 year old for each county, entered into CSV
# Verified number by county with KFF map
# https://public.tableau.com/profile/kaiser.family.foundation#!/vizhome/InsurerParticipationinthe2017IndividualMarketplace/2017InsurerParticipation
####################################################################################

md17 <- read.csv("data-original/state-based/raw/2017-md-insurers-raw.csv", stringsAsFactors = F)

md17 <- left_join(md17, fips_md, by = "county_name")
md17 <- md17 %>% mutate(year = 2017,
												state_code = "MD") %>%
	arrange(fips_county) %>%
	select(year, fips_county, county_name, state_code, everything())

write.csv(md17, "data-original/state-based/2017-md-insurers.csv", row.names = F, na = "")

# ####################################################################################
# Join annual issuers by county data
# 2016 from CMS's SBM PUFs processed in prepSbmPuf.R
# ####################################################################################

# Use 2015 data sent by state
md15 <- md %>% filter(year == 2015)
md17 <- as.data.frame(md17)

# Join to 2017 data
md_use <- bind_rows(md15, md17)
md_use <- md_use %>% select(-name_match)
write.csv(md_use, "data-original/state-based/md-insurers.csv", row.names = F, na = "")

####################################################################################
# County enrollment
# 2017 http://www.marylandhbe.com/wp-content/uploads/2017/01/020117_OE4Count.pdf
####################################################################################

md_enroll17 <- read.csv("data-original/state-based/raw/tabula-2017-md-enrollment-020117_OE4Count.csv", stringsAsFactors = F)
# Interested in QHPs only
md_enroll17 <- md_enroll17[,c(1,2)]
# Format
md_enroll17 <- md_enroll17 %>% rename(plan_selections = PRIVATE.QUALIFIED.HEALTH.PLANS) %>%
	mutate(plan_selections = as.numeric(str_replace_all(plan_selections, ",", ""))) %>%
	filter(county_name != "Out Of State" & county_name != "TOTAL") %>%
	mutate(county_name = ifelse(str_detect(county_name, "City"), county_name,
															paste(county_name, "County", sep = " ")))
md_enroll17 <- left_join(md_enroll17, fips_md, by="county_name")

md_enroll17 <- md_enroll17 %>% mutate(year = 2017, state_code = "MD") %>%
	select(year, fips_county, county_name, everything()) %>%
	arrange(year, fips_county)

write.csv(md_enroll17, "data-original/state-based/md-county-enrollment.csv", row.names = F, na = "")
