
# historic to 2020
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2001tomid2020detailedtimeseries/ukdetailedtimeseries2001to2020.zip"
destfile <- "pop_est.zip"
curl::curl_download(url, destfile)

pop_est <- read.csv(unz("pop_est.zip", "MYEB1_detailed_population_estimates_series_UK_(2020_geog20).csv"))

unlink("pop_est.zip")

# projections

url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/localauthoritiesinenglandtable2/2018based/table2.xls"
destfile <- "pop_proj.xls"
curl::curl_download(url, destfile)

pop_proj <- read_excel("pop_proj.xls", sheet = "Persons", 
                                 skip = 6, .name_repair = "universal")

unlink("pop_proj.xls")


#Cut out years I don't need, and LAs
pop_est <-
  pop_est %>% 
  filter(laname20 %in% unique(animal_dt$District)) %>% 
  select(laname20, starts_with("population_201"), population_2020,
         -population_2010, -population_2011, -population_2012)


#sum up and pivot
pop <- pop_est %>% 
  group_by(laname20) %>% 
  summarise(across(population_2013:population_2020, sum)) 


# NOw same idea for projections

pop_2 <- pop_proj %>% 
  filter(AREA %in% unique(animal_dt$District)) %>% 
  select(AREA
         , population_2021 = ...2021
         , population_2022 = ...2022
         , population_2023 = ...2023) %>% 
  group_by(laname20 = AREA) %>% 
  summarise(across(population_2021:population_2023, sum))

# Combine and pivot
pop <- 
  pop %>% 
  inner_join(pop_2) %>% 
  pivot_longer(-laname20)
