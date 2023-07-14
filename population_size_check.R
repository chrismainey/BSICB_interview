pop_est %>% 
  group_by(laname20) %>% 
  summarise(across(population_2013:population_2020, sum)) %>% 
  filter(laname20 %in% c("Birmingham", "Solihull", "Dudley", "Walsall", "Sandwell", "Coventry", "Wolverhampton"))


pop_proj %>% 
  filter(AGE.GROUP == "All ages") %>%
  group_by(AREA) %>% 
  summarise(across(...2018:...2030, sum)) %>% 
 
  filter(AREA %in% c("Birmingham", "Solihull", "Dudley", "Walsall", "Sandwell", "Coventry", "Wolverhampton"))


pop_proj %>% 
  distinct(AGE.GROUP)
