# Here is where we do the maps
library(sp)
# Load library
library(sf)

# Load shapefile

shapename_19<- st_read("./Wards_LAD_V2/WD_DEC_2019_UK_BFE_LAD.shp")
shapename_11<- st_read("./Wards_LAD_V2/Wards_December_2011_FEB_EW_LAD.shp")

shape_WM <- 
  shapename_19 %>% 
  filter(LAD19NM %in% unique(animal_dt$District))

shape_WM_11 <- 
  shapename_11 %>% 
  filter(lad11nm %in% unique(animal_dt$District))


unique(shape_WM$LAD19NM)
unique(shape_WM_11$lad11nm)

# get right shape first
ggplot() + 
  geom_sf(data = shape_WM
          , size = 1.5
          , color = "black"
          , fill = "cyan1") + 
  ggtitle("Wards") + 
  coord_sf()


typeof(shape_WM)
typeof(map_set)

#join on ward?
# Need to remove the work 'Ward' to match ONS shape file.
# WMFAS said they don't know when meta data is changed over as they rely on IT to do it.
# I've made a guess at 2018 here, after a lot of manual checking and back and forth I've cleaned up below.
a<- animal_dt %>% 
  filter(dt >= "2018-04-01") %>% 
  mutate(year = year(dt)) %>% 
  group_by(District, Ward, year) %>% 
  mutate(Rescues = n(),
         Ward = str_remove(Ward, " Ward")) %>% 
  left_join(shape_WM, c("Ward" = "WD19NM"))

# Difference around including . in some St. names between 11 and 19.
shape_WM_11 <-
  shape_WM_11 %>% 
  mutate(wd11nm = 
           case_when(
                     
                      wd11nm == "St Michael's" ~ "St. Michael's" 
                      , wd11nm == "St Pauls" ~ "St. Pauls" 
                      , wd11nm == "St Peter's" ~ "St. Peter's"
                      , .default = wd11nm))

# Manual fix to two bad entries in sources data for St Michaels in Coventry.
animal_dt <-
  animal_dt %>% 
  mutate(Ward = ifelse((Ward == "St Michael's" & District =="Coventry")
                       , "St. Michael's"
                       , Ward)
         )


b<- animal_dt %>% 
  filter(dt < "2018-04-01") %>% 
  mutate(year = year(dt)) %>% 
  group_by(District, Ward, year) %>% 
  mutate(Rescues = n(),
         Ward = str_trim((str_remove(Ward, " Ward")))) %>% 
  left_join(shape_WM_11, c("Ward" = "wd11nm"))


# Combine two parts together
map_set <- 
  select(a, Incdate, year, District, Ward, Incident.Detail, dt, Rescues, geometry) %>% 
  bind_rows(select(b, Incdate, year, District, Ward, Incident.Detail, dt, Rescues, geometry))


theme_map <-function(){
  theme_void()+
    theme(plot.background = element_rect(fill = "#F0F4F5", colour = "#F0F4F5")
          , panel.background =element_rect(fill = "#F0F4F5", colour = "#F0F4F5")
          , panel.border = element_blank()
          , plot.title = element_text(family="Open Sans", face =  "bold", size = 16)
          , plot.subtitle = element_text(family="Open Sans", face =  "italic", size = 8)
          , legend.title=element_text(size=10)
    )
  
}


library(gganimate)
library(transformr)

# expand the WM set to match the mapping by year.
yrs1 <- seq(2013, 2023)
yrs1 <- seq(2018, 2023)

map_set_agg <- 
  map_set %>% 
  group_by(District, Ward, geometry) %>% 
  summarise(Rescues = sum(Rescues))

map_set_agg %>% 
  summarise(max(Rescues))


# get right shape first
map1<- ggplot() + 
  geom_sf(data = shape_WM, aes(geometry = geometry)
          , size = 1.5
          , color = "black"
          #, fill = "cyan1"
          , 
  ) + 
  geom_sf(data = map_set_agg, aes(geometry = geometry, fill= Rescues)
          , size = 1.5
          , color = "black"
          #, fill = "cyan1"
  ) + 
  
  geom_sf(data = map_LA_agg, aes(geometry = geometry)
          , size = 2
          , color = "white"
          , fill = NA
  ) + 
  geom_sf_text(data = shape_WM_LA, aes(label = LAD19NM), col="white", size=3.5)+
  
  scale_fill_viridis_c(alpha=0.5)+
  #transition_time(year) +
  labs(title ="Animal Rescuse by Ward 2013 - 2023"
            , subtitle = "Data Source: https://www.cityobservatory.birmingham.gov.uk/@west-midlands-fire-service/animal-rescues") + 
  coord_sf()+
  theme_map()


map1

ragg::agg_png("./outputs/ward_map.png" , width = 758, height = 471, units = "px")
map1
dev.off()

# get right shape first
yearsplt<- ggplot() + 
  geom_sf(data = shape_WM, aes(geometry = geometry)
          , size = 1.5
          , color = "black"
          #, fill = "cyan1"
          , 
  ) + 
  geom_sf(data = map_set, aes(geometry = geometry, fill= Rescues)
          , size = 1.5
          , color = "black"
          #, fill = "cyan1"
          , alpha=0.5
          ) + 
  
  geom_sf(data = map_LA_agg, aes(geometry = geometry)
          , size = 3
          , color = "white"
          , fill = NA
  ) + 
  geom_sf_text(data = shape_WM_LA, aes(label = LAD19NM), col="black", size=2.7, fontface="bold")+
  
  scale_fill_viridis_c(alpha = 0.5, na.value = 0)+
  transition_time(year) +
  #facet_wrap(~year)+
  labs(title ="Animal Rescuse by Ward {frame_time}"
       , subtitle = "Data Source: https://www.cityobservatory.birmingham.gov.uk/@west-midlands-fire-service/animal-rescues") + 
  coord_sf()+
  theme_void()+
  theme(plot.background = element_rect(fill = "#F0F4F5", colour = "#F0F4F5")
        , panel.background =element_rect(fill = "#F0F4F5", colour = "#F0F4F5")
        , panel.border = element_blank()
        , plot.title = element_text(family="Open Sans", face =  "bold", size = 12)
        , plot.subtitle = element_text(family="Open Sans", face =  "italic", size = 6)
        , legend.title=element_text(size=9)
  )

#yearsplt

num_years <- max(map_set$year) - min(map_set$year)+1
animate(yearsplt, nframes = num_years, duration = 20, device = "png")

anim_save("./outputs/anim_wards_year.gif")




