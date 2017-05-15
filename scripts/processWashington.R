# Washington data

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# fips codes
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_wa <- fips_codes %>% filter(state_code == "WA") %>%
	select(fips_county, county_name)

####################################################################################
# 2016-2017 Plan data (list form) from
# https://www.wahbexchange.org/new-customers/coverage-basics/plans/#2017
####################################################################################

# 2017
# Read in list data - one row per insurer, with county names listed after
wa_raw17 <- read_lines("data-original/state-based/raw/2017-wa-insurers-raw.txt")
wa_raw17 <- as.data.frame(wa_raw17)

# Separate out county name
wa_raw17 <- wa_raw17 %>% separate(col = wa_raw17, into = c("issuer_name", "counties"), sep = " – ", extra = "merge")
# One county per line
wa17 <- wa_raw17 %>% mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(state_code = "WA",
				 year = 2017,
				 county_name = str_trim(county_name, "both"))

# 2016
wa_raw16 <- read_lines("data-original/state-based/raw/2016-wa-insurers-raw.txt")
wa_raw16 <- as.data.frame(wa_raw16)
wa_raw16 <- wa_raw16 %>% separate(col = wa_raw16, into = c("issuer_name", "counties"), sep = " – ", extra = "merge")

# "All 39 counties" - need to list out
nameswa <- paste(c(fips_wa$county_name), collapse=', ' )
temp <- fips_wa$county_name[fips_wa$county_name != "Clark"]
nameswa_noclark <- paste(c(temp), collapse=', ' )
wa_raw16 <- wa_raw16 %>% mutate(counties = ifelse(counties == "All 39 counties", nameswa,
																									ifelse(counties == "All counties except Clark", nameswa_noclark, counties)))
# One county per line
wa16 <- wa_raw16 %>% mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(state_code = "WA",
				 year = 2016,
				 county_name = str_trim(county_name, "both"))

####################################################################################
# 2014 and 2015 provided via email
####################################################################################

wa_raw14 <- read.csv("data-original/state-based/raw/2014-wa-insurers-raw.csv", stringsAsFactors = F)
wa14 <- wa_raw14 %>% mutate(counties = ifelse(counties == "All counties", nameswa,
																									ifelse(counties == "All counties except Clark", nameswa_noclark, counties)))%>% 
	mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(state_code = "WA",
				 year = 2014,
				 county_name = str_trim(county_name, "both"))

wa_raw15 <- read.csv("data-original/state-based/raw/2015-wa-insurers-raw.csv", stringsAsFactors = F)
wa15 <- wa_raw15 %>% mutate(counties = ifelse(counties == "All counties", nameswa,
																									ifelse(counties == "All counties except Clark", nameswa_noclark, counties))) %>% 
	mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(state_code = "WA",
				 year = 2015,
				 county_name = str_trim(county_name, "both"))

####################################################################################
# Join, save
####################################################################################
wa <- rbind(wa14, wa15, wa16, wa17)

wa <- wa %>% mutate(county_name = paste(county_name, "County", sep = " "))
wa <- left_join(wa, fips_wa, by = "county_name")
wa <- wa %>%
	select(year, fips_county, state_code, county_name, everything()) %>%
	arrange(fips_county)

write.csv(wa, "data-original/state-based/wa-insurers.csv", row.names = F, na = "")

####################################################################################
# County enrollment
# 2017 data sent via email - represents effectuated enrollment?
####################################################################################
wa_enroll17 <- read.csv("data-original/state-based/raw/2017-wa-county-enrollment-raw.csv", stringsAsFactors = F)
wa_enroll17 <- wa_enroll17 %>% mutate(county_name = paste(county_name, "County", sep = " "))
wa_enroll17 <- left_join(wa_enroll17, fips_wa, by = "county_name")

# Add fields for joining
wa_enroll17 <- wa_enroll17 %>% mutate(state_code = "WA", year = 2017) %>%
	select(year, fips_county, county_name, state_code, everything()) %>%
	rename(plan_selections = effectuated_enrollment)
write.csv(wa_enroll17, "data-original/state-based/wa-county-enrollment.csv", row.names = F, na = "")
