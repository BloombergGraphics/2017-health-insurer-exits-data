# Get HIOS ids and names especially for SBMs
# Note: this should NOT!!! be considered a comprehensive list
# Insurers don't usually have to file if increase <10%
# For healthcare.gov state id+names, always use data from healthcare.gov landscape/PUF files

library(dplyr)
library(tidyr)

####################################################################################
# Download united rate review files
# Files updated 11/04/16
# Source: https://www.cms.gov/CCIIO/Resources/Data-Resources/ratereview.html
####################################################################################

# 2017
download.file("https://www.cms.gov/CCIIO/Resources/Data-Resources/Downloads/2017_PUF_20161103.zip", destfile="data-original/hios/2017_PUF_20161103.zip")
unzip("data-original/hios/2017_PUF_20161103.zip", exdir = "data-original/hios/")

# 2016
download.file("https://www.cms.gov/CCIIO/Resources/Data-Resources/Downloads/2016_PUF_20161103.zip", destfile="data-original/hios/2016_PUF_20161103.zip")
unzip("data-original/hios/2016_PUF_20161103.zip", exdir = "data-original/hios/")

# 2015
download.file("https://www.cms.gov/CCIIO/Resources/Data-Resources/Downloads/2015_PUF_20161103.zip", destfile="data-original/hios/2015_PUF_20161103.zip")
unzip("data-original/hios/2015_PUF_20161103.zip", exdir = "data-original/hios/")

# 2014
download.file("https://www.cms.gov/CCIIO/Resources/Data-Resources/Downloads/2014_PUF_20161103.zip", destfile="data-original/hios/2014_PUF_20161103.zip")
unzip("data-original/hios/2014_PUF_20161103.zip", exdir = "data-original/hios/")

####################################################################################
# Assemble URR file
####################################################################################

urr17 <- read.csv("data-original/hios/2017 PUF/WKSH1_PUF_2017_20161103.csv", stringsAsFactors = F)
urr16 <- read.csv("data-original/hios/2016 PUF/WKSH1_PUF_2016_20161103.csv", stringsAsFactors = F)
urr15 <- read.csv("data-original/hios/2015 PUF/WKSH1_PUF_2015_20161103.csv", stringsAsFactors = F)
urr14 <- read.csv("data-original/hios/2014 PUF/WKSH1_PUF_2014_20161103.csv", stringsAsFactors = F)

processUrr <- function(dt, year) {
	colnames(dt) <- tolower(colnames(dt))
	dt <- dt %>% select(state, market, company, issuer_id) %>%
		rename(state_code = state) %>%
		mutate(year = year) %>%
		arrange(state_code, issuer_id)
}

urr17 <- processUrr(urr17, 2017)
urr16 <- processUrr(urr16, 2016)
urr15 <- processUrr(urr15, 2015)
urr14 <- processUrr(urr14, 2014)

urr <- rbind(urr14, urr15, urr16, urr17) %>%
	arrange(state_code, issuer_id, year)

# Some duplicates, group
urr <- unique(urr)

####################################################################################
# Get 2016 SBM PUF HIOS ids
# File created in prepSbmPuf.R
# This still isn't complete - missing a DC company at least, and is only one year
####################################################################################
sbmpuf <- read.csv("data-original/landscape/2016_QHP_Landscape_SBM_Individual_Market_Medical_allstates.csv", stringsAsFactors = F)

sbmid <- sbmpuf %>% select(year, state_code, issuer_id, issuer_name) %>%
	group_by(year, state_code, issuer_id, issuer_name) %>%
	summarize(temp = n()) %>%
	select(-temp) %>%
	arrange(state_code, issuer_id)

####################################################################################
# Single reference file
####################################################################################
sbmid <- sbmid %>% mutate(market = "Individual", source = "SBM PUF")
urr <- urr %>% mutate(source = "URR") %>%
	rename(issuer_name = company)
sbmid <- as.data.frame(sbmid)
ids <- rbind(sbmid, urr) %>%
	arrange(state_code, issuer_id, year)

write.csv(ids, "data/hios-ids.csv", na ="", row.names = F)
