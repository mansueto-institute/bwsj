
library(tidyverse)
library(ggmap)
library(sf)
library(tidygeocoder)
library(tidycensus)
library(scales)
library(readxl)
library(viridis)


# Setup steps -------------------------------------------------------------

# Obtain Census API Key here: https://api.census.gov/data/key_signup.html
#census_api_key('API_KEY', install = TRUE) 
readRenviron("~/.Renviron")

# Download data from Box here: https://uchicago.box.com/s/ut4wyfluhtdm3j8b1fd6ek3l05jtf890
# Put it in a local folder and enter path here:
file_path = '/Users/nm/Desktop/BWSJ/'

# Read in data from the downloaded BWSJ folder
mwdbe <- read_csv(paste0(file_path,'mwdbe.csv'))
if (file.exists(paste0(file_path,'mwdbe_clean.geojson'))) { mwdbe_clean <- st_read(paste0(file_path,'mwdbe_clean.geojson')) }
mapcorps <- read_csv(paste0(file_path,'Map Corps Chicago 2009-2020/Chicago_Data_2019.csv'))
chi_buildings <- st_read(paste0(file_path,'chicago_footprints.geojson'))
infogroup <- read_csv(paste0(file_path,'chicago_establishments.csv'))
afam_biz_licenses <- read_csv(paste0(file_path,'afam_biz_licenses.csv'))

# MWDBE Data --------------------------------------------------------------
# Source: https://chicago.mwdbe.com/ 

# Clean up the data
mwdbe <- mwdbe %>% 
  # Removes spaces and non-alphanumeric characters in columns names
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% 
  # Converts all columns names to lower case
  rename_all(list(tolower)) %>% 
  # Removes non-numeric characters from industry columns
  mutate_at(vars(capability, commodity_codes), ~ gsub('[^0-9-]', '', .)) %>% 
  # Substring first 6 characters from each industry columns and coalesces into single column
  mutate(industry_code = coalesce(str_sub(commodity_codes,1,6),str_sub(capability,1,6))) 

# Filter to only African Americans
unique(mwdbe$ethnicity) # Look at values of ethnicity column
mwdbe <- mwdbe %>% filter(ethnicity %in% c("African-American (Black)", "African American"))

# Geocode addresses to lat - lons using a free API service from Census and OSM
# WARNING TAKES 10 minutes - if-else logic to skips this step if you have the file already downloaded from box
if (file.exists(paste0(file_path,'mwdbe_clean.geojson'))) {
  mwdbe_clean <- st_read(paste0(file_path,'mwdbe_clean.csv'))
} else {
  mwdbe <- mwdbe %>% 
    geocode(street = physical_address, city = city, state = state, postalcode = zip,
            method = "cascade", cascade_order = c("census","osm"))
  
  # Select columns with potentially useful data
  mwdbe_clean <- mwdbe %>% 
    select(company_name, 
           owner_first, owner_last, 
           physical_address, city, state, zip, phone, email, industry_code, 
           lat, long) 
  
  mwdbe_clean <- mwdbe_clean %>% mutate(source = 'mwdbe') %>%
    # Standardize / rename column names 
    rename(name = company_name, 
           street_address = physical_address,
           naics6 = industry_code,
           latitude = lat,
           longitude = long) %>% 
    # Drop NAs
    drop_na(latitude, longitude) %>% 
    # Convert to lat lons to geometry format
    st_as_sf(coords = c("longitude", "latitude"), 
             crs = 4326, agr = "constant")
  
  # Download and read in Census NAICS Codes
  naics_url <- 'https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
  tmp_filepath <- paste0(tempdir(), '/', basename(naics_url))
  download.file(url = paste0(naics_url), destfile = tmp_filepath)
  naics <- read_excel(tmp_filepath)
  
  # Clean up columns
  naics_clean <- naics %>%
    select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
    rename_all(list(tolower)) %>%
    select(seq__no_, `2017_naics_us_code`, `2017_naics_us_title`) %>%
    drop_na() %>%
    rename(naics_code = `2017_naics_us_code`,
           naics_title = `2017_naics_us_title`)
  
  # Add in NAICS Code Label
  mwdbe_clean <- mwdbe_clean %>%
    mutate(naics4 = str_sub(gsub('-','',naics6),1,4)) %>% 
    left_join(., naics_clean, by = c('naics4'='naics_code'))
}

# Create unique ID
mwdbe_clean <- mwdbe_clean %>%
  arrange(phone, email, name, street_address) %>%
  mutate(mwdbe_id = row_number(),
         universe_id = paste0(source,"_",mwdbe_id)) 

# MapCorps ----------------------------------------------------------------

mapcorps_clean <- mapcorps %>% 
  rename_all(list(tolower)) %>%
  # Drop out of business establishments
  filter(placestatus != "Out of Business") %>% 
  mutate(street_address = paste0(paste0(buildingnumber,' ',street,", ",unit))) %>% 
  # Combine fields into street address
  select(name,
         street_address,city,zip,
         placestatus, placetype, placesubtype, privateresidence, mappedyear, phonenumber, 
         lat, lng) %>%
  arrange(name, street_address, phonenumber, lat, lng) %>%
  # Create unique ID
  mutate(mapcorps_id = row_number()) 


mapcorps_clean <- mapcorps_clean %>% 
  mutate(source = 'mapcorps',
         universe_id = paste0(source,"_",mapcorps_id)) %>% 
  # Standardize / rename column names 
  rename(latitude = lat, 
         longitude = lng) %>% 
  # Drop NAs
  drop_na(latitude, longitude) %>% 
  # Convert to lat lons to geometry format
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")


# InfoGroup ---------------------------------------------------------------

# Select useful columns
infogroup_clean <- infogroup %>%
  select(abi, company,
         address_line_1, city, state, zipcode,
         primary_naics_code, #yellow_page_code, archive_version_year, 
         employee_size_5_location, sales_volume_9_location, year_established, # business_status_code, 
         subsidiary_number, parent_number,
         latitude, longitude) %>%
  # Select first 6 numbers in NAICS code
  mutate(naics6 = str_sub(primary_naics_code,1,6)) 

infogroup_clean <- infogroup_clean %>% 
  mutate(source = 'infogroup',
         universe_id = paste0(source,"_",abi)) %>%
  # Standardize / rename column names 
  rename(name = company, 
         street_address = address_line_1,
         zip = zipcode) %>% 
  # Drop NAs
  drop_na(latitude, longitude) %>%  
  # Convert to lat lons to geometry format
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")

# Business Licenses -------------------------------------------------------

# Download African American Business License Account Numbers
# Nico matched business owners names in the Chicago Data Portal to a voterfile within Cook County. There were 7,766 good matches.

# Download Business License data from Chicago Data Portal 
biz_licenses <- read_csv('https://data.cityofchicago.org/api/views/uupf-x98q/rows.csv?accessType=DOWNLOAD')
biz_licenses <- biz_licenses %>%
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% 
  rename_all(list(tolower)) %>%
  filter(city == 'CHICAGO')

# Check if file is in folder
if (file.exists(paste0(file_path,'biz_licenses_geocode.csv'))) {
  biz_licenses_geocode <- read_csv(paste0(file_path,'biz_licenses_geocode.csv'))
} else {
  # If not in folder geocode missing lat lons
  biz_licenses_geocode <- biz_licenses %>% 
    filter(is.na(latitude)) %>%
    geocode(street = address, city = city, state = state, postalcode = zip_code,
            method = "cascade", cascade_order = c("census","osm")) %>%  
    # Coalesce missing columns with geocoded columns
    mutate(latitude = coalesce(latitude,lat),
           longitude = coalesce(longitude,long)) %>%
    # Select everything except for these columns
    select(-one_of(c('lat','long','geo_method')))
}
  
biz_licenses <- biz_licenses %>% 
  drop_na(latitude, longitude) %>%  
  # Stack / row bind the dataframes together
  rbind(., biz_licenses_geocode ) %>% 
  drop_na(latitude, longitude) %>%  
  # Convert to lat lons to geometry format
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")

biz_licenses_clean <- biz_licenses %>% 
  # Inner join and limit to universe of AfAm owned businesses and standardize 
  inner_join(., afam_biz_licenses, by=c('account_number'='account_number')) %>%
  select(id, license_id, account_number, site_number, legal_name, doing_business_as_name, 
         address, city, state, zip_code, 
         license_code, license_description, business_activity_id, business_activity, match_label, geometry) %>%
  # Rename column names 
  rename(street_address = address,
         zip = zip_code,
         name = doing_business_as_name) %>%
  # Change from character to numeric
  mutate(zip = as.numeric(zip)) %>%
  mutate(source = 'licenses',
         universe_id = paste0(source,"_",license_id)) 
  
# Combine all business files ----------------------------------------------

# Check for duplicates
mwdbe_clean %>%
  group_by(mwdbe_id) %>%
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

biz_licenses_clean  %>%
  group_by(license_id) %>%
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

infogroup_clean  %>% 
  group_by(abi) %>% 
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

mapcorps_clean  %>% 
  group_by(mapcorps_id) %>% 
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

# Combine files into master file: 

full_universe <- bind_rows(mwdbe_clean,biz_licenses_clean,infogroup_clean,mapcorps_clean) 

full_universe  %>% 
  group_by(universe_id) %>% 
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

full_universe_match_job <- full_universe %>%
  select(universe_id,
         name,
         street_address,
         city,
         state,
         zip,
         source,
         geometry) %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%
  st_drop_geometry()

write_csv(full_universe_match_job, paste0(file_path,'match_job.csv'))

# Submit Match Job Here: https://shop.safegraph.com/match/

# Read in output from the match job


# Community Areas ---------------------------------------------------------

# Download Community Area geometries
community_areas <- sf::st_read('https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON') %>% 
  st_as_sf() %>% 
  select(community)

# Join Community Areas to Universe
full_universe <- full_universe %>%
  st_join(., community_areas)

# Spatial and Census data ---------------------------------------------------------

# Download Chicago tract geometries
chicago_tracts <- sf::st_read('https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON') %>% 
  st_as_sf() %>%
  mutate_at(vars(geoid10), list(as.character)) %>%
  mutate(geoid10 = str_pad(geoid10, width=11, side="left", pad="0")) %>%
  select(geoid10) %>%
  rename(geoid = geoid10)

# Investigate Census Variables
acs5_vars <- load_variables(year = 2019, dataset = 'acs5', cache = FALSE)
acs5_vars_subject <- load_variables(year = 2019, dataset = 'acs5/subject', cache = FALSE)
# Variables for Population and Median Household Income for Black and all races
acs5_vars_selected <- c('B02009_001', 'B02001_001', 'B19013_001', 'B19013B_001', 'B25001_001')

# Download Census data by tract for Cook County IL
acs_tract <- get_acs(year = 2019, geography = "tract", survey = 'acs5', variables = acs5_vars_selected, state = '17', county = '031')
# Reshape the table from long to wide
acs_tract <- acs_tract %>%
  rename_all(list(tolower)) %>%
  pivot_wider(id_cols = geoid,
            names_from = c(variable), 
            values_from = c(estimate)) %>%
  rename(total_housing_units = B25001_001, # TOTAL HOUSING UNITS
         total_population = B02001_001, # TOTAL POPULATION
         black_population = B02009_001, # POPULATION OF BLACK OR AFRICAN AMERICAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES
         median_household_income = B19013_001, # MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS
         black_median_household_income = B19013B_001) # MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS) BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER

chicago_acs_tract <-chicago_tracts %>%
  left_join(., acs_tract, by = c('geoid'='geoid')) %>% # table join dataframes together on geoid
  st_join(., community_areas, left= TRUE, largest = TRUE) %>% # spatial join data on lat lon
  filter(!is.na(community)) %>% # filter where community is missing
  mutate(black_population_share = black_population/total_population)

# Visually inspect the data
ggplot(chicago_acs_tract, aes(fill = black_population_share, color = black_population_share)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

ggplot(chicago_acs_tract, aes(fill = median_household_income, color = median_household_income)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

ggplot(chicago_acs_tract, aes(fill = log(black_median_household_income), color = log(black_median_household_income) )) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

# Aggregate from tract to community area
chicago_acs_community_areas <- chicago_acs_tract %>%
  st_drop_geometry()%>%
  # Create weighted columns for median household income
  mutate(black_median_household_income = coalesce(black_median_household_income, median_household_income),
         median_household_income_weighted = total_housing_units*median_household_income,
         total_black_housing_units = total_housing_units*black_population_share,
         black_median_household_income_weighted = black_median_household_income*total_black_housing_units) %>%
  # Group by community and aggregate
  group_by(community) %>%  
  summarise_at(vars(total_population,
                    black_population,
                    median_household_income_weighted,
                    black_median_household_income_weighted,
                    total_black_housing_units,
                    total_housing_units), list(sum), na.rm = TRUE) %>% 
  ungroup() %>%
  # Adjust median estimates
  mutate(black_median_household_income = median_household_income_weighted / total_black_housing_units,
         median_household_income = median_household_income_weighted / total_housing_units) %>%
  select(community, 
         total_population,
         median_household_income,
         black_population,
         black_median_household_income) %>%
  mutate(black_population_share = black_population/total_population)

# Join community data to community area geometries
chicago_acs_community_areas <- left_join(community_areas, chicago_acs_community_areas, by = c('community'='community')) %>%
  st_transform(crs = st_crs(4326)) %>% 
  st_as_sf()

# Visually inspect
ggplot(chicago_acs_community_areas, aes(fill = black_population_share, color = black_population_share)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

ggplot(chicago_acs_community_areas, aes(fill = median_household_income, color = median_household_income)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

ggplot(chicago_acs_community_areas, aes(fill = log(black_median_household_income), color = log(black_median_household_income) )) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 
