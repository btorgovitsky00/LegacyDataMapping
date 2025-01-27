Legacy Data Mapping - Skeleton
================
Bryson Torgovitsky
1/27/2025

``` r
#If necessary, parameters for limiting range of maps
lon_min <- 'number1'
lon_max <- 'number2'
lat_min <- 'number3'
lat_max <- 'number4'

#Calling the database. Tracking number of observations throughout clarifies the impact of each refining step
Database1_Raw <- read.csv("####") #number of observations

#Removing unnecessary columns identified from the raw dataframe
Database1_Clean <- Database1_Raw[ -c()] #number of observations

#Refining data to preset longitude/latitude range, and renaming columns to consistent format
Database1_Local <- subset(Database1_Clean, (decimalLatitude > lat_min & decimalLatitude < lat_max &
                                decimalLongitude > lon_min & decimalLongitude < lon_max)) %>% dplyr::rename(
              "Prefecture" = "stateProvince", "Country" = "country",
              "Precise Location" = "county", "Longitude" = "decimalLongitude", "Latitude" = "decimalLatitude",
              "Year" = "year", "Genus" = "genus", "Source" = "institutionCode", "Family" = "family"
              ) #number of observations

#Adding an additional column for blocks of time based preferred timescale, using "Year" here
breaks <- c() #Ranges of timeframe

labels <- c() #Title of each range

Database1_Local$Timeframe <- cut(Database1_Local$Year, breaks, labels)

#Adding column for binomial nomenclature based on Genus and Species information available, can be omitted if the information is already present. This method enables comparison of species names used between databases via the object "ClaSciNameDb#"
ClaSciNameDb1 <- paste(Database1_Local$Genus, Database1_Local$specificEpithet, Database1_Local$infraspecificEpithet, sep=' ')

Database1_Local <- cbind(Database1_Local, ClaSciNameDb1)

#Renaming species column to a consistent format
Database1_Local <- Database1_Local[ -c(5:6)] %>%  dplyr::rename(
              "SciName" = "ClaSciNameInvOct")

#Removing datapoints unusable for mapping

Database1_Local$SciName <- sub("\\s+(?:NA)$", "", Database1_Local$SciName) #number of observations

Database1_Local <- replace(Database1_Local, Database1_Local=='', NA) #number of observations

Database1_Local <- Database1_Local[!is.na(Database1_Local$Source),] #number of observations

Database1_Local <- Database1_Local[!is.na(Database1_Local$Year),] #number of observations

Database1_Local <- Database1_Local[!is.na(Database1_Local$Family),] #number of observations


#Preparing for datapoint mapping 
world_map <- map_data("world")

region <- map_data("world", region="")


#Base code for map at designated longitude/latitude range
ggplot(data = world_map, aes(long, lat)) +
  geom_polygon(aes(group=group)) +
  coord_map(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max))

#Map with specimen collection datapoints
ggplot(data = world_map, aes(long, lat)) +
  geom_polygon(aes(group=group)) +
  coord_map(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max)) +
  geom_point(data = Database1_Local, aes(x = Longitude, y = Latitude, color = Database1_Local$Family
  )) + 
   scale_colour_discrete(drop=TRUE,
    limits = levels(Database1_Local$Family)) +
  facet_wrap(~ImpCal, nrow = 2)

#Identifying unique source codes (useful for databases with multiple dataframes)
unique(Database1_Local$Source)
```

``` r
#Calling the database
Database2_Raw <- read.csv("####") #number of observations

#Removing unnecessary columns identified from the raw dataframe
Database2_Clean <- Database2_Raw[ -c()] #number of observations

#Refining data to preset longitude/latitude range, and renaming columns to consistent format
Database2_Local <- subset(Database2_Clean, (decimalLatitude > lat_min & decimalLatitude < lat_max &
                                decimalLongitude > lon_min & decimalLongitude < lon_max)) %>% dplyr::rename(
              "Prefecture" = "stateProvince", "Country" = "country",
              "Precise Location" = "county", "Longitude" = "decimalLongitude", "Latitude" = "decimalLatitude",
              "Year" = "year", "Genus" = "genus", "Source" = "institutionCode", "Family" = "family"
              ) #number of observations

#Adding an additional column for blocks of time based preferred timescale, using "Year" here
breaks <- c() #Ranges of timeframe

labels <- c() #Title of each range

Database2_Local$Timeframe <- cut(Database2_Local$Year, breaks, labels)

#Adding column for binomial nomenclature based on Genus and Species information available, can be omitted if the information is already present. This method enables comparison of species names used between databases

ClaSciNameDb2 <- paste(Database2_Local$Genus, Database2_Local$specificEpithet, Database2_Local$infraspecificEpithet, sep=' ')

Database2_Local <- cbind(Database2_Local, ClaSciNameDb2)

#Renaming species column to a consistent format, removing Genus/Specific epithet columns (optional)
Database2_Local <- Database2_Local[ -c(5:6)] %>%  dplyr::rename(
              "SciName" = "ClaSciNameInvOct")

#Removing datapoints unusable for mapping

Database2_Local$SciName <- sub("\\s+(?:NA)$", "", Database2_Local$SciName) #number of observations

Database2_Local <- replace(Database2_Local, Database2_Local=='', NA) #number of observations

Database2_Local <- Database2_Local[!is.na(Database2_Local$Source),] #number of observations

Database2_Local <- Database2_Local[!is.na(Database2_Local$Year),] #number of observations

Database2_Local <- Database2_Local[!is.na(Database2_Local$Family),] #number of observations

#Map with specimen collection datapoints
ggplot(data = world_map, aes(long, lat)) +
  geom_polygon(aes(group=group)) +
  coord_map(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max)) +
  geom_point(data = Database2_Local, aes(x = Longitude, y = Latitude, color = Database1_Local$Family
  )) + 
   scale_colour_discrete(drop=TRUE,
    limits = levels(Database2_Local$Family)) +
  facet_wrap(~ImpCal, nrow = 2)

#Identifying unique source codes (useful for databases with multiple dataframes)
unique(Database2_Local$Source)
```

``` r
#Calling the database
Database3_Raw <- read.csv("####") #number of observations

#Removing unnecessary columns identified from the raw dataframe
Database3_Clean <- Database3_Raw[ -c()] #number of observations

#Refining data to preset longitude/latitude range, and renaming columns to consistent format
Database3_Local <- subset(Database3_Clean, (decimalLatitude > lat_min & decimalLatitude < lat_max &
                                decimalLongitude > lon_min & decimalLongitude < lon_max)) %>% dplyr::rename(
              "Prefecture" = "stateProvince", "Country" = "country",
              "Precise Location" = "county", "Longitude" = "decimalLongitude", "Latitude" = "decimalLatitude",
              "Year" = "year", "Genus" = "genus", "Source" = "institutionCode", "Family" = "family"
              ) #number of observations

#Adding an additional column for blocks of time based preferred timescale, using "Year" here
breaks <- c() #Ranges of timeframe

labels <- c() #Title of each range

Database3_Local$Timeframe <- cut(Database3_Local$Year, breaks, labels)

#Adding column for binomial nomenclature based on Genus and Species information available, can be omitted if the information is already present. This method enables comparison of species names used between databases

ClaSciNameDb2 <- paste(Database3_Local$Genus, Database3_Local$specificEpithet, Database3_Local$infraspecificEpithet, sep=' ')

Database3_Local <- cbind(Database3_Local, ClaSciNameDb2)

#Renaming species column to a consistent format, removing Genus/Specific epithet columns (optional)
Database3_Local <- Database3_Local[ -c(5:6)] %>%  dplyr::rename(
              "SciName" = "ClaSciNameInvOct")

#Removing datapoints unusable for mapping

Database3_Local$SciName <- sub("\\s+(?:NA)$", "", Database3_Local$SciName) #number of observations

Database3_Local <- replace(Database3_Local, Database3_Local=='', NA) #number of observations

Database3_Local <- Database3_Local[!is.na(Database3_Local$Source),] #number of observations

Database3_Local <- Database3_Local[!is.na(Database3_Local$Year),] #number of observations

Database3_Local <- Database3_Local[!is.na(Database3_Local$Family),] #number of observations

#Map with specimen collection datapoints
ggplot(data = world_map, aes(long, lat)) +
  geom_polygon(aes(group=group)) +
  coord_map(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max)) +
  geom_point(data = Database3_Local, aes(x = Longitude, y = Latitude, color = Database1_Local$Family
  )) + 
   scale_colour_discrete(drop=TRUE,
    limits = levels(Database3_Local$Family)) +
  facet_wrap(~Timeframe, nrow = 2)

#Identifying unique source codes (useful for databases with multiple dataframes)
unique(Database3_Local$Source)
```

``` r
museum_all <- rbind(Database1_Local, Database2_Local, Database3_Local) #number of observations

#Removes potentially confounding formatting from catalog numbers
museum_all$catalogNumber <- gsub("[^0-9.-]", "", as.character(museum_all$catalogNumber))

#Optional, removes specimens with inconclusive identifications (e.g., "Genus sp.")
museum_all$SciName <- stringr::str_remove(museum_all$SciName, pattern = "")

#Removes duplicate specimens by catalog number
museum_all <- museum_all %>% group_by(catalogNumber) %>% filter(duplicated(catalogNumber) | n()==1) #number of observations

museum_all <- museum_all[!duplicated(museum_all$catalogNumber, incomparables = NA), ] #number of observations

museum_all <- museum_all %>% mutate(individualCount = ifelse(is.na(individualCount), 1, individualCount)) #number of observations

#Optional, duplicates by the number of specimens present in a lot. Useful if ascertaining the number of specimens of a given species captured total or at specific sampling events
museum_all <- museum_all[rep(row.names(museum_all), museum_all$individualCount), 1:13] #number of observations

#Map with specimen collection datapoints
ggplot(data = world_map, aes(long, lat)) +
  geom_polygon(aes(group=group)) +
  coord_map(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max)) +
  geom_point(data = museum_all, aes(x = Longitude, y = Latitude, color = Database1_Local$Family
  )) + 
   scale_colour_discrete(drop=TRUE,
    limits = levels(museum_all$Family)) +
  facet_wrap(~Timeframe, nrow = 2)

#Identifying unique source codes (useful for databases with multiple dataframes)
unique(museum_all$Source)
```

``` r
museum_spec <- museum_all #number of observations

#Optional, removes specimens with inconclusive identifications (e.g., "Genus sp.")
museum_spec <- museum_spec[!grepl("", museum_spec$SciName),] #number of observations

#General refinement, identifies remaining incomplete entries and extra spaces in binomial nomenclature
museum_spec <- replace(museum_spec, museum_spec==' ', NA)

museum_spec$SciName <- str_squish(museum_spec$SciName)

museum_spec <- museum_spec[!is.na(museum_spec$SciName),] #number of observations

museum_spec <- museum_spec[!is.na(museum_spec$Genus),] #number of observations


#Optional, removes specimens with inconclusive identifications (e.g., "Genus sp.")
museum_spec <- museum_spec[!grepl("",museum_spec$SciName),] #number of observations

museum_spec <- museum_spec[!grepl("",museum_spec$SciName),] #number of observations

#------#Species name fixes-----

#Update species to correct taxonomy from selected taxonomic database. Useful for outdated taxonomies, misspellings, etc.

#Identifying unique species (useful for identifying misspellings)
unique(museum_spec$SciName)

#Genus species1

#Number of specimens pre-correction
length(which(museum_spec$SciName=="Genus species1")) #number of observations

#Object which contains all alternative taxonomies
G_species1 <- c("Genus species|Genus speceis1|Genos species1|Genus species2")

#Updates incorrect entries to accepted taxonomy
museum_spec$SciName <-  str_replace_all(museum_spec$SciName, G_species1, 'Genus species1')

#Number of specimens post-correction
length(which(museum_spec$SciName=="Genus species1")) #number of observations

#Update "Genus" column according to the updated species information
museum_spec$Genus <- word(museum_spec$SciName, 1)

#Update families to include only accepted genuses after Genus update
museum_spec$Family <- case_when(
  museum_spec$Genus == 'Genus 1' ~"Family 1",
  museum_spec$Genus == 'Genus 2' | museum_spec$Genus == 'Genus 3' ~ 'Family 2',
  museum_spec$Genus == 'Genus 4' | museum_spec$Genus == 'Genus 5' |  museum_spec$Genus == 'Genus 6' ~ 'Family 3',
)

#---------------------------

#Optional, remove Genus not included by the study. Can be changed to any taxonomic level by changing the selected column
museum_spec <- museum_spec[!(museum_spec$Genus %in% c("","")),] #number of observations

#Italicize the binomial nomenclature!
museum_spec$SciName <- substitute(paste(italic(museum_spec$SciName)))

ggplot(data = world_map, aes(long, lat)) +
  geom_polygon(aes(group=group)) +
  coord_map(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max)) +
  geom_point(data = museum_spec, aes(x = Longitude, y = Latitude, 
    fill = museum_spec$Genus, color = museum_spec$Genus, shape = museum_spec$Family), size=2) + #Point color by Genus and shape by Family, can be changed to any taxonomic level
  labs(color='Genus', shape = "Family") + #Labels for color and shape in the legend, change accordingly if using another value
  theme(legend.text = element_text(face = "italic"),
        text= element_text(size=30)) + #Italicizing legend text (optional) and adjusting text size for visibility
   guides(fill="none") + 
  labs(x = "Longitude", y = "Latitude") + #X and Y Axes labels
   scale_colour_discrete(drop=TRUE,
    limits = levels(museum_spec$Genus)) + #Color by Genus, change accordingly if coloring by another value
  scale_shape_manual(values = c(15,16,17,18,25)) + #Custom shapes for Family, improves consistency between figures
  facet_wrap(~TimeFrame, nrow = 2) #Individual graphs by selected timeframe

#Number of Family 1, Number of Family 2, Number of Family 3 

#Object which lists each species identified
museum_specName <- unique(museum_spec$SciName)


#Obtain counts of each species identified
museum_specSciName<- museum_spec[ -c(2:3, 5:10)]

Timeframe_count <- museum_specSciName %>%
  count(Timeframe,name = "Count") #Number in time1, Number in time2, Number in time3

#Output counts
head(Timeframe_count)

SciName_count <- museum_specSciName %>%
  count(SciName,name = "Count")

#Object which lists each genus identified
museum_genName <- unique(museum_spec$Genus) 
```

``` r
#Isolate species of interest, can adjust according to taxonomic level by changing selected column
museum_target <- museum_spec[museum_spec$SciName %in% c("",""),] #number of observations

#Add additional nomenclature (useful if including names in other languages)
museum_target$AltName <- case_when(
  museum_target$SciName == 'Genus species1' ~ "A",
  museum_target$SciName == 'Genus species2' ~ 'B',
  museum_target$SciName == 'Genus species3' ~ 'C'
)

ggplot(data = world_map, aes(long, lat)) +
  geom_polygon(aes(group=group)) +
  coord_map(xlim=c(lon_min, lon_max), ylim=c(lat_min, lat_max)) +
  geom_point(data = museum_target, aes(x = Longitude, y = Latitude, 
    fill = museum_target$Genus, color = museum_target$Genus, shape = museum_target$Family), size=2) + #Point color by Genus and shape by Family, can be changed to any taxonomic level
  labs(color='Genus', shape = "Family") + #Labels for color and shape in the legend, change accordingly if using another value
  theme(legend.text = element_text(face = "italic"),
        text= element_text(size=30)) + #Italicizing legend text (optional) and adjusting text size for visibility
   guides(fill="none") + 
  labs(x = "Longitude", y = "Latitude") + #X and Y Axes labels
   scale_colour_discrete(drop=TRUE,
    limits = levels(museum_target$Genus)) + #Color by Genus, change accordingly if coloring by another value
  scale_shape_manual(values = c(15,16,17,18,25)) + #Custom shapes for Family, improves consistency between figures
  facet_wrap(~TimeFrame, nrow = 2) #Individual graphs by selected timeframe


#Object which lists each species identified
Target_spec <- museum_target %>%
  count(SciName,name = "Count")

#Object which lists each species identified by timeframe
Target_timeframe <- museum_target %>%
  count(Timeframe,name = "Count") 

#Output counts
head(Target_spec)

head(Target_timeframe)
```

Note that the `echo = TRUE` parameter was added to the code chunk to
enable printing of the R code that generated the plot.`eval = MUTE` was
added to enable knitting of this file.
