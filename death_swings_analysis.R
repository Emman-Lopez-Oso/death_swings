library(tidyverse)
#library(tidycensus)
library(mapview)
library(tigris)
library(sf)
library(lwgeom)
#library(geojsonsf)
#library(Hmisc)
library(stringr)
#library(vroom)
library(plyr)

# Data from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7760H
# Covid Deaths: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/

# I/O ---- 
# Read in NYT precincts data 
precincts_loc <- "~/Dropbox (ECONW)/Personal/Research Projects/Death Swings/data/dataverse_files/"
# Read in COVID Deaths data 
deaths <- read.csv("~/Dropbox (ECONW)/Personal/Research Projects/Death Swings/data/covid_deaths_usafacts.csv")

# read in TIGRIS Line data 
usa <- counties()


## Create usa precincts shapefiles ----
datalist = list()
for(i in 1:length(list.files(precincts_loc))) {
  # Create a variable for the  file that is being read in 
  read_f_ <- list.files(precincts_loc)[i]
  # assign the variable temp1 to the read file
  temp1 <- paste0(precincts_loc,read_f_)
  temp2 <- tempfile()
  
  #unzip the contents in 'temp' and save unzipped content in 'temp2'
  unzip(zipfile = temp1, exdir = temp2)
  #finds the filepath of the shapefile (.shp) file in the temp2 unzip folder
  #the $ at the end of ".shp$" ensures you are not also finding files such as .shp.xml
  your_SHP_file<-list.files(temp2, pattern = ".shp$",full.names=TRUE)
  #read the shapefile. Alternatively make an assignment, such as f<-sf::read_sf(your_SHP_file)
  shp_file <- st_read(your_SHP_file) %>% 
    st_transform(st_crs(usa)) 
  # Save the newly made shapefile in the datalist
  datalist[[i]] <- shp_file
}

# Function does not work well with large dataframes ----
# rbind.match.columns <- function(input1, input2) {
#   n.input1 <- ncol(input1)
#   n.input2 <- ncol(input2)
#   
#   if (n.input2 < n.input1) {
#     TF.names <- which(names(input2) %in% names(input1))
#     column.names <- names(input2[, TF.names])
#   } else {
#     TF.names <- which(names(input1) %in% names(input2))
#     column.names <- names(input1[, TF.names])
#   }
#   
#   return(rbind(input1[, column.names], input2[, column.names]))
# } 

# Use plyr's rbind.fill to combine dataframes ----
all_us = do.call(rbind.fill, datalist)

# Unload plyr because it's an asshole
detach("package:plyr", unload=TRUE)

# Convert into an sf object
all_us <- st_as_sf(all_us) %>% 
  st_zm()

# Data Cleaning ----
## Deaths Data ----
# Remove the rest of the data that is fairly unnecessary 
deaths <- deaths %>% 
  select(countyFIPS,County.Name,State,StateFIPS,X2022.01.14)

# add leading zeros to county fips 
deaths$countyFIPS <- str_pad(deaths$countyFIPS,5,pad="0")

# Join deaths to the usa file 
usa_deaths <- usa  %>% 
  left_join(deaths, by=c("GEOID"="countyFIPS"))

## Precincts Data -----

# Create a table that shows the number of null values in each column 
null_vals <- data.frame(t(colSums(is.na(all_us %>% st_drop_geometry()))))

#write_csv(null_vals,"null_values.csv")

# create a dataframe out of the identification columns in the all_us df
us_elec_id_cols <- 
  all_us %>% select(
    DISTRICT,NAME,STATEFP20,COUNTYFP20,VTDST20,GEOID20,
    NAME20,STATE_FIPS,COUNTY_FIP,COUNTY_NAM,PRECINCT,CDE_COUNTY,PCTNUM,PRECINCTNA,
    COUNTY,CNTY_CODE,FIPS_CODE,SRPREC_KEY,SRPREC,ADDIST,CDDIST,SDDIST,BEDIST,
    STATEFP,COUNTYFP,VTDST,GEOID,pct_std,county,precinct,CTYSOSID,PRECINCT_I,
    PRECINCT_N,CTYNAME,CTYNUMBER,CTYNUMBER2,FIPS2,C20PRERTRU,C20PREDBID,C20PRELJOR,
    NAMELSAD,WP_NAME,WARD,TOWN,TOWN_ID,NUMBER,JURSCODE,VOTESPRE,COUNTY20,PRECINCTID,
    COUNTYFIPS,cousubname,elexpre,VTDID,PCTNAME,COUNTYNAME,STATEFP10,COUNTYFP10,
    SOSPRECINC,PREC_ID,ENR_DESC,COUNTY_ID,NAMELSAD20,CONGR_DIST,MUN_NAME,PRECINCT20,
    PCT_CEB,STATE,PCODE,CODE_NAME,CNTY,COLOR,PREC,PCTKEY,CNTYKEY,CountyID,vistapre,
    resultspre,LOCALITY,PRECCODE,ST_CODE,PRECNAME,CNTY_FIPS,CNTY_NAME,COUSUBFP,
    MCD_FIPS,MCD_NAME,CTV,LABEL,ASM,SEN,CON
  ) %>% st_drop_geometry()
# create function reduce that uses the unname function to remove values that 
# are not null, and creates a list of values from all of the columns 
reduce <- function(x) unname(x[!is.na(x)])
# Create a new column that is a list of values from the column above
us_elec_id_cols$all_ids <- apply(us_elec_id_cols, 1, reduce)

# Get sum of total votes for presidential Election 
all_us <- all_us %>% 
  select(starts_with("G20PRE")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(tot_pres_votes = rowSums(across(where(is.numeric))))
  
# Get proportions of four main parties:
all_us <- all_us %>% 
  mutate(votes_dem_pct = G20PREDBID/tot_pres_votes,
         votes_rep_pct = G20PRERTRU/tot_pres_votes,
         votes_gre_pct = G20PREGHAW/tot_pres_votes, #Green party votes
         votes_lib_pct = G20PRELJOR/tot_pres_votes)

# Rename Columns from Presidential Election  ----
all_us <- all_us %>% rename(Trump_Rv = G20PRERTRU,
                  Biden_Dv = G20PREDBID,
                  Jorgen_Lv = G20PRELJOR,
                  Blanken_Constv = G20PRECBLA,
                  Pierce_Indv = G20PREIPIE,
                  Fuente_Alliancev = G20PREOFUE, 
                  Hawkins_Greenv = G20PREGHAW,
                  Carrol_AmSolidv = G20PREACAR)
# Create new subset of columns for all US 
all_us_join <- 
  all_us %>% 
  select(
    Biden_Dv,
    Jorgen_Lv,
    Blanken_Constv,
    Pierce_Indv,
    Fuente_Alliancev, 
    Hawkins_Greenv,
    Carrol_AmSolidv,
    votes_dem_pct,
    votes_rep_pct,
    votes_gre_pct, #Green party votes
    votes_lib_pct,
    tot_pres_votes)


# Spatial Join ---- 
# I realized that I can't really join the precincts results to the death counts
# through regular table joins because the data is pretty much all 
# over the place in terms of whether or not it has FIPS info, etc.
sf::sf_use_s2(FALSE)

death_swings_df <- usa_deaths %>%
  select(STATEFP,COUNTYFP,GEOID,NAME,X2022.01.14) %>% 
  st_join(all_us %>% 
            st_point_on_surface(),
          left = TRUE)
names(death_swings_df)

death_swings_group <- 
  death_swings_df %>% st_drop_geometry() %>% 
  group_by(GEOID) %>% 
  summarise(
    state_fp = first(STATEFP),
    cnty_fp = first(COUNTYFP),
    geoid = first(GEOID),
    tot_death = first(X2022.01.14),
    tot_pres_votes= sum(tot_pres_votes),
    Biden_Dv = sum(Biden_Dv),
    Trump_Rv = sum(Trump_Rv),
    Jorgen_Lv = sum(Jorgen_Lv),
    Hawkins_Greenv = sum(Hawkins_Greenv)
    
  )


death_swings_group <-  
  death_swings_group %>% 
  mutate(votes_dem_pct = Biden_Dv/tot_pres_votes,
         votes_rep_pct = Trump_Rv/tot_pres_votes,
         votes_gre_pct = Hawkins_Greenv/tot_pres_votes, #Green party votes
         votes_lib_pct = Jorgen_Lv/tot_pres_votes)

swing_counties <- death_swings_group %>% filter(between(votes_dem_pct,.47,.53))

swing_counties <- swing_counties %>% mutate(pct_death_votes = tot_death/tot_pres_votes)

view(swing_counties %>% filter(pct_death_votes>.01))

# So it doesn't look like any swing precincts would be possible. But how about
# the house of representatives? 
con_dis <-congressional_districts()

# Join the death swings group to the USA counties data 
deaths_votes_county_shp <- usa %>% select("STATEFP"  ,"COUNTYFP" ,"COUNTYNS", "GEOID") %>% 
  left_join(death_swings_group,by = "GEOID")

# make the 




