# Common functions used in marketplace data analsyis

library(dplyr)
library(tidyr)
library(stringr)

####################################################################################
# Get HIOS ids for SBM matching, from makeHiosIds.R
####################################################################################
getHiosIds <- function(state_abbrev) {
	ids <- read.csv("data/hios-ids.csv", stringsAsFactors = F)
	ids_state <- ids %>% filter(state_code == state_abbrev & market=="Individual") %>%
		group_by(issuer_id, issuer_name) %>%
		summarize(temp = n()) %>%
		select(-temp)
	return(ids_state)
}

getHiosFull <- function(state_abbrev) {
	ids <- read.csv("data/hios-ids.csv", stringsAsFactors = F)
	ids_state <- ids %>% filter(state_code == state_abbrev & market=="Individual") %>%
		mutate(issuer_name = str_replace_all(issuer_name, "  ", ", ")) %>%
		arrange(issuer_id, year)
}