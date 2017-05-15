# Connecticut data
# Every county is its own rating area, plans offered statewide

library(dplyr)
library(tidyr)
library(stringr)

source("scripts/commonFunctions.R")
ids_ct <- getHiosIds("CT")

# Get county fips and names
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_ct <- fips_codes %>% filter(state_code == "CT") %>%
	select(fips_county, county_name, state_code)

####################################################################################
# 2017 Plans from Access Health CT - browsed as a 27 year old for each county
# Anthem BCBS offers 12 and ConnectiCare offers 7 plans in each

# 2016 plans from http://www.ct.gov/cid/lib/cid/ratechart.pdf
# [2015 open enrollment report](http://shadac.org/sites/default/files/CT.Enrollment.2.1.16%202015%20Review.pdf)
# [2014 enrollment report](http://www.shadac.org/sites/default/files/Old_files/shadac/publications/CT_BoardofDirectorsSlides_5.22.14.pdf)
####################################################################################
ct17 <- data.frame(c("Anthem Health Plans, Inc.", "ConnectiCare Benefits, Inc."), c(12, 7))
colnames(ct17) <- c("issuer_name", "plans")
ct17 <- ct17 %>% mutate(state_code = "CT", year = 2017)

ct16 <- data.frame(c("Anthem Health Plans, Inc.", "ConnectiCare Benefits, Inc.", "HealthyCT, Inc.", "UnitedHealthcare Insurance Company"))
colnames(ct16) <- c("issuer_name")
ct16 <- ct16 %>% mutate(state_code = "CT", year = 2016)

ct15 <- data.frame(c("Anthem Health Plans, Inc.", "ConnectiCare Benefits, Inc.", "HealthyCT, Inc.", "UnitedHealthcare Insurance Company"))
colnames(ct15) <- c("issuer_name")
ct15 <- ct15 %>% mutate(state_code = "CT", year = 2015)

ct14 <- data.frame(c("Anthem Health Plans, Inc.", "ConnectiCare Benefits, Inc.", "HealthyCT, Inc."))
colnames(ct14) <- c("issuer_name")
ct14 <- ct14 %>% mutate(state_code = "CT", year = 2014)

ct <- bind_rows(ct14, ct15, ct16, ct17)
ct <- left_join(fips_ct, ct, by = "state_code")
ct <- ct %>% select(year, fips_county, everything())

# Add issuer IDs
ct <- left_join(ct, ids_ct, by="issuer_name")
# Connecticare not joining, add manually
ct <- ct %>% mutate(issuer_id = ifelse(issuer_name == "ConnectiCare Benefits, Inc.", 76962, issuer_id))

write.csv(ct, "data-original/state-based/ct-insurers.csv", row.names = F, na = "")

####################################################################################
# Enrollment
# 2017 enrollment %s from slide 20 http://agency.accesshealthct.com/wp-content/uploads//2017/02/AHCT-2017-Open-Enrollment-Summary-Report-1.pdf
####################################################################################
ct_enroll17 <- read.csv("data-original/state-based/raw/2017-ct-county-enrollment-raw.csv")
# Total QHP enrollment: 111542
ct_stateenroll <- 111542
ct_enroll17 <- ct_enroll17 %>% mutate(plan_selections = round(enrollment_share * ct_stateenroll),
																			county_name = paste(county_name, "County", sep = " ")) %>%
	select(-enrollment_share)

# Add fips
ct_enroll17 <- left_join(ct_enroll17, fips_ct, by="county_name")
ct_enroll17 <- ct_enroll17 %>% mutate(year = 2017) %>%
	select(year, fips_county, everything()) %>%
	arrange(year, fips_county)

write.csv(ct_enroll17, "data-original/state-based/ct-county-enrollment.csv", row.names = F, na = "")
