# Format plan filings downloaded from SERFF
options(java.parameters = "-Xmx1024m")
library(XLConnect)
library(dplyr)
library(tidyr)
library(stringr)

####################################################################################
# Definition datasets
####################################################################################
# Fips
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_codes <- fips_codes %>% select(fips_county, state_code, county_name)

# Issuer names
issuer_names <- read.csv("data-original/issuer-names.csv", colClasses = "character")

####################################################################################
# Functions
####################################################################################
# Service areas
readSerffSa <- function(dirpath, files, state_code, year) {
	sa <- NULL
	for (f in seq_along(files)) {
		wb <- loadWorkbook(paste(dirpath, files[f], sep = ""))
		print(files[f])
		
		# metadata
		column_names <- as.vector(readWorksheet(wb, sheet = "Service Areas", header = F, startRow = 11, endRow = 11)[1,])
		issuer_id <- readWorksheet(wb, sheet = "Service Areas", header = F, startRow = 6, endRow = 6, startCol = 2, endCol = 2)[1,1]
		
		# Read worksheet, assing colnames
		temp <- readWorksheet(wb, sheet = "Service Areas", header = F, startRow = 13)
		if (dim(temp)[2] < dim(column_names)[2]) {
			num <- dim(temp)[2]
			colnames(temp) <- as.vector(column_names[1, c(1:num)])
		} else {
			colnames(temp) <- as.vector(column_names[1,])
		}
		temp <- temp %>% mutate(issuer_id = issuer_id, state_code = state_code, year = year)
		sa <- bind_rows(sa, temp)
	}
	colnames(sa) <- gsub('[.]', '_',tolower(colnames(sa)))
	colnames(sa) <- gsub('[*]', '_',colnames(sa))
	colnames(sa) <- gsub(' ', '_',colnames(sa))
	colnames(sa) <- gsub('[(]', '',colnames(sa))
	colnames(sa) <- gsub('[)]', '',colnames(sa))
	colnames(sa) <- gsub("_$", "",colnames(sa))
	
	# Binary vars
	sa[sa == "Yes"] <- 1
	sa[sa == "No"] <- 0
	sa <- sa %>% rename(cover_entire_state = state)
	
	#if ("county_name" %in% colnames(sa)) {
	# Separate out county names
	#	sa <- sa %>% separate(county_name, into = c("county_name", "fips_county"), sep = " - ", fill="warn")
	#}
	sa <- sa %>% separate(county_name, into = c("county_name", "fips_county"), sep = " - ", fill="warn")
	
	# For full state service areas, make one row per county
	fips_list <- fips_codes[which(fips_codes$state_code == state_code),]$fips_county
	sa <- sa %>% select(-county_name) %>%
		mutate(fips_county = ifelse(cover_entire_state == 1, paste(c(fips_list), collapse=', ' ), fips_county)) %>%
		mutate(fips_county = strsplit(fips_county, ",")) %>% 
		unnest(fips_county) %>%
		mutate(fips_county = str_trim(fips_county, side="both"))
	
	# Add nice county names
	sa <- left_join(sa, fips_codes, by = c("fips_county", "state_code"))
	sa <- sa %>% select(year, state_code, issuer_id, fips_county, county_name, everything())
	
	return(sa)
}

# Plan benefits
readSerffBenefits <- function(dirpath, files, year) {
	plans <- NULL
	for (f in seq_along(files)) {
		wb <- loadWorkbook(paste(dirpath, files[f], sep = ""))
		print(files[f])
		
		# For each "Benefits Package" sheet append the data
		benefits_sheets <- getSheets(wb)[grepl("Benefits Package", getSheets(wb))]
		for (s in seq_along(benefits_sheets)) {
			issuer_id <- readWorksheet(wb, sheet = benefits_sheets[s], header = F, startRow = 2, endRow = 2, startCol = 2, endCol = 2)[1,1]
			state_code <- readWorksheet(wb, sheet = benefits_sheets[s], header = F, startRow = 3, endRow = 3, startCol = 2, endCol = 2)[1,1]
			print(benefits_sheets[s])
			temp <- readWorksheet(wb, sheet = benefits_sheets[s], header = T, startRow = 8, endRow = 58)
			temp <- temp %>% mutate(state_code = state_code, issuer_id = issuer_id)
			plans <- bind_rows(plans, temp)
		}
	}
	# Format joined dataset
	colnames(plans) <- gsub('[.]', '_',tolower(colnames(plans)))
	colnames(plans) <- gsub('[*]', '_',colnames(plans))
	colnames(plans) <- gsub(' ', '_',colnames(plans))
	colnames(plans) <- gsub("___", "_",colnames(plans))
	colnames(plans) <- gsub("__", "_",colnames(plans))
	colnames(plans) <- gsub("_$", "",colnames(plans))
	plans <- plans %>% mutate(year = year) %>%
		select(year, state_code, issuer_id, everything())
	return(plans)
}

# Single dataset of QHPs by county
makeSerffDataset <- function(dirpath, state_code, year) {
	benefits_files <- list.files(path = dirpath, pattern = "_DATA_BENEFITS.xlsm$", recursive = TRUE)
	sa_files <- list.files(path = dirpath, pattern = "_SERVICE_AREA.xls$", recursive = TRUE)
	
	plans <- readSerffBenefits(dirpath, benefits_files, year)
	plans <- left_join(plans, issuer_names, by=c("state_code", "issuer_id"))
	
	sa <- readSerffSa(dirpath, sa_files, state_code, year)
	qhp <- left_join(plans, sa, by=c("year", "state_code", "issuer_id", "service_area_id")) %>%
		select(year, state_code, issuer_id, issuer_name, fips_county, county_name, everything()) %>%
		filter(qhp_non_qhp != "Off the Exchange") %>%
		# Rename plan_brochure to match FFM files
		rename(plan_brochure_url = plan_brochure)
	return(qhp)
}

####################################################################################
# Colorado
# https://filingaccess.serff.com/sfa/home/CO
# individual medical, excluding withdrawn or off-tabexchange-only binders
# Oregon 2014 plans
# https://filingaccess.serff.com/sfa/home/OR
####################################################################################

co_qhp14 <- makeSerffDataset("data-original/serff/co-2014/", "CO", 2014)
or_qhp14 <- makeSerffDataset("data-original/serff/or-2014/", "OR", 2014)
mn_qhp14 <- makeSerffDataset("data-original/serff/mn-2014/", "MN", 2014)

qhp14 <- rbind(co_qhp14, or_qhp14, mn_qhp14)
write.csv(qhp14, "data-original/landscape/2014_QHP_Landscape_SBM_Individual_Market_Medical.csv", row.names = F, na = "")

co_qhp15 <- makeSerffDataset("data-original/serff/co-2015/", "CO", 2015)
write.csv(co_qhp15, "data-original/landscape/2015_QHP_Landscape_SBM_Individual_Market_Medical.csv", row.names = F, na = "")
