# Marketplace enrollment data by county

library(dplyr)
library(tidyr)

ffm <- read.csv("data/healthcaregov-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
ffm <- ffm %>% filter(fips_county != "Unsuppressed Total")

####################################################################################
# Add state-based marketplaces
# From state sources, but verified with Federal OEP state-level report (some state totals slightly differ due to rounding etc)
####################################################################################

ca <- read.csv("data-original/state-based/ca-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
co <- read.csv("data-original/state-based/co-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
ct <- read.csv("data-original/state-based/ct-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
dc <- read.csv("data-original/state-based/dc-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
id <- read.csv("data-original/state-based/id-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
ma <- read.csv("data-original/state-based/ma-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
md <- read.csv("data-original/state-based/md-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
mn <- read.csv("data-original/state-based/mn-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
ny <- read.csv("data-original/state-based/ny-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
ri <- read.csv("data-original/state-based/ri-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
vt <- read.csv("data-original/state-based/vt-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))
wa <- read.csv("data-original/state-based/wa-county-enrollment.csv", stringsAsFactors = F, colClasses=c("fips_county" = "character"))

enrollment <- bind_rows(ffm, ca, co, ct, dc, id, ma, md, mn, ny, ri, vt, wa)

####################################################################################
# Join
####################################################################################

enrollment <- enrollment %>% select(year, fips_county, county_name, state_code, everything()) %>%
	arrange(year, fips_county) %>%
	mutate(percent_aptc = plan_selections_aptc/plan_selections)

write.csv(enrollment, "data/county-enrollment.csv", row.names = F, na = "")
