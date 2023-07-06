
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
  pivot_longer(-laname20) %>% 
  mutate(year = as.numeric(substring(name, nchar(name)-3, nchar(name))))


weighted_summary <- 
  animal_dt %>% 
  group_by(District, year) %>% 
  summarise(Rescues = n()) %>% 
  left_join(pop, by= join_by(District == laname20, year == year)) %>% 
  mutate(prop = 1000 * Rescues/value)

# Coventry notably lower
total_weighted <-
  weighted_summary %>% 
  group_by(District) %>% 
  summarise(Rescues = sum(Rescues),
            popn = sum(value),
            prop = 1000 * sum(Rescues)/sum(value)
  )

# plot for proportions
total_weighted %>% 
  select(District, "Rescues Per 1000 population" = prop, "Total Rescues" = Rescues) %>% 
  pivot_longer(-District, names_to = "Measure") %>% 
  ggplot(aes(x=District, fill = District, group=Measure))+
  geom_col(aes(y=value), col=1, alpha=0.7, linewidth = 0.5)+
  #geom_col(aes(y=Rescues), position =  position_nudge(x = 0.3),  width = 0.5)+
  #scale_y_continuous(sec.axis = ~ .)+
  scale_fill_viridis_d()+
  facet_wrap(~Measure, scales = "free_y")+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, angle=45, hjust = 1),
        axis.title = element_blank())



ggsave("./outputs/raw_vs_weighted.png",  device = png, type = "cairo", dpi = 300,
       width = 4, height = 3, units = "in")

animal_dt %>% 
  distinct(year)

pop %>% 
  distinct(year)
