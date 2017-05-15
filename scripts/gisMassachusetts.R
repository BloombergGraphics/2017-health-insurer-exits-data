# Massachusetts GIS munging
# MA rating areas based on 3-digit zips
# Most accurate zip map from http://www.mass.gov/anf/research-and-tech/it-serv-and-support/application-serv/office-of-geographic-information-massgis/datalayers/zipcodes.html

library(dplyr)
library(rgdal)
library(stringr)

download.file("http://wsgw.mass.gov/data/gispub/shape/state/zipcodes_nt.zip", destfile = "documents/ma/zipcodes_nt.zip")
unzip("documents/ma/zipcodes_nt.zip", exdir = "documents/ma/zipcodes_nt/")

####################################################################################
# Extract zip3-county data from shapefile
####################################################################################
mazip <- readOGR("documents/ma/zipcodes_nt/", "ZIPCODES_NT_POLY")
zipdt <- as.data.frame(mazip@data)

colnames(zipdt) <- tolower(colnames(zipdt))
head(zipdt)
zipdt[] <- lapply(zipdt, as.character)

zipdt <- zipdt %>% mutate(zip3 = str_trunc(postcode, 3, "right", ellipsis = "")) %>%
	mutate(county = str_to_title(county))

zipcounty <- zipdt %>% group_by(zip3, county) %>%
	summarize(temp = n()) %>%
	select(-temp)

write.csv(zipdt, "data-original/state-based/raw/ma-zipcodes.csv", na="", row.names = F)

# Add rating areas to county file
ma_ra <- read.csv("data-original/state-based/2017-ma-ratingareas.csv", colClasses = c("zip3" = "character"))
zipcounty <- left_join(zipcounty, ma_ra, by="zip3")
zipcounty <- zipcounty %>% mutate(state_code = "MA") %>%
	rename(county_name = county) %>%
	arrange(county_name, rating_area)

write.csv(zipcounty, "data-original/state-based/raw/ma-zipcounties.csv", na="", row.names = F)

# Add zip3 to shapefile for future mapping
mazip@data$POSTCODE <- as.character(mazip@data$POSTCODE)
mazip@data$ZIP3 = str_trunc(mazip@data$POSTCODE, 3, "right", ellipsis = "")
writeOGR(mazip, dsn = "documents/ma/zipcodes_edit/", layer = "zipcodes", driver = "ESRI Shapefile")
