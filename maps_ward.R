# Here is where we do the maps
library(sf)
library(sp)
# Load library


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
# Need to remove the word 'Ward' to match ONS shape file.
# WMFAS said they don't know when meta data is changed over as they rely on IT to do it.
# I've made a guess at 2018 here, after a lot of manual checking and back and forth I've cleaned up below.
a<- animal_dt %>% 
  filter(dt >= "2018-04-01") %>% 
  mutate(Ward = str_remove(Ward, " Ward")) %>% 
  # group_by(District, Ward, fyear, geometry) %>% 
  # summarise(Rescues = n()) %>% 
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
  mutate(Ward = str_remove(Ward, " Ward")) %>% 
  # group_by(District, Ward, fyear) %>% 
  #summarise(Rescues = n()) %>%
  left_join(shape_WM_11, c("Ward" = "wd11nm"))


# Combine two parts together
map_set <- 
  select(a, Incdate, fyear, District, Ward, Incident.Detail, dt, geometry) %>% 
  bind_rows(select(b, Incdate, fyear, District, Ward, Incident.Detail, dt, geometry))

# UPDATE: where polygon and multiploygon, chose multipolygon
geomtry_join_temp <-
  map_set %>% 
  group_by(District, Ward, geometry) %>% 
  summarise(ct = n()) %>% 
  #mutate(as.character(unlist(geometry)))
  filter(ct > 1 & st_geometry_type(geometry) == "MULTIPOLYGON")

map_set <- 
  map_set %>% 
  left_join(geomtry_join_temp, by=c("District", "Ward")) %>% 
  mutate(geometry = st_sfc(ifelse(st_is_empty(geometry.y), st_cast(geometry.x, "MULTIPOLYGON"), geometry.y))) %>% 
  select(-geometry.x, -geometry.y, -ct)

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


# Use the maps set for aggregates of wards because of the name cleaning and reconcilliation.
map_set_agg <- 
  map_set %>% 
  group_by(District, Ward, geometry) %>% 
  summarise(Rescues = n())

map_set_agg %>% 
  filter(Rescues >9) %>% 
  arrange(desc(Rescues))

  

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
          , linewidth = 1.2
          , color = "black"
          , fill = NA
  ) + 
  geom_sf_label(data = shape_WM_LA, aes(label = LAD19NM), col="black", size=3, alpha=0.6)+
  
  scale_fill_viridis_c(alpha=0.5)+
  #transition_time(year) +
  labs(title ="Animal Rescues by Ward, for fiscal years 2013/14 - 2022/23"
            , subtitle = "Data Source: https://www.cityobservatory.birmingham.gov.uk/@west-midlands-fire-service/animal-rescues") + 
  coord_sf()+
  theme_map()+
  theme(legend.position = c(0.8,0.75))


map1

ragg::agg_png("./outputs/ward_map.png" , width = 758, height = 471, units = "px")
map1
dev.off()

# By year
map_set_agg_fyear <- 
  map_set %>% 
  group_by(District, Ward, fyear, geometry) %>% 
  summarise(Rescues = n())

yearsplt<- 
  ggplot() + 
  geom_sf(data = shape_WM, aes(geometry = geometry)
          , size = 1.5
          , color = "black"
          #, fill = "cyan1"
          , 
  ) + 
  geom_sf(data = map_set_agg_fyear, aes(geometry = geometry, fill= Rescues)
          , size = 1.5
          , color = "black"
          #, fill = "cyan1"
          , alpha=0.5
          ) + 
  
  geom_sf(data = map_LA_agg, aes(geometry = geometry)
          #, size = 5
          , linewidth = 1.2
          , color = "black"
          , fill = NA
  ) + 
  geom_sf_label(data = head(shape_WM_LA,7), aes(label = LAD19NM), col="black", size=2
                , fontface="bold", alpha=0.6)+
  
  scale_fill_viridis_c(alpha = 0.5, na.value = 0)+
  transition_states(fyear, transition_length = 0, state_length = 1) +
  #facet_wrap(~year)+
  labs(title ="Animal Rescues by Ward, for {next_state}"
       , subtitle = "Data Source: https://www.cityobservatory.birmingham.gov.uk/@west-midlands-fire-service/animal-rescues") + 
  coord_sf()+
  theme_void()+
  theme(plot.background = element_rect(fill = "#F0F4F5", colour = "#F0F4F5")
        , panel.background =element_rect(fill = "#F0F4F5", colour = "#F0F4F5")
        , panel.border = element_blank()
        , plot.title = element_text(family="Open Sans", face =  "bold", size = 12)
        , plot.subtitle = element_text(family="Open Sans", face =  "italic", size = 6)
        , legend.title=element_text(size=7)
        , legend.text=element_text(size=6)
        , legend.position = c(0.8,0.75)
  )

#yearsplt

num_years <- as.numeric(length(levels(map_LA_agg_yr$fyear)))
animate(yearsplt, nframes = num_years, duration = 15, device = "png")

anim_save("./outputs/anim_wards_year.gif")




# Need to trim white space off image and gif




# blank space 

image <- image_read('./outputs/anim_district_year.gif')

# Printing the image
print(image, info = FALSE)

image_info(image)

r3<-image_crop(image = image, geometry = "480x340+0+68")
print(r3, info = FALSE)

image_write(r3, path = './outputs/anim_district_year2.gif')

# blank space 
# Next

image <- image_read('./outputs/anim_wards_year.gif')

# Printing the image
print(image, info = FALSE)

image_info(image)

r4<-image_crop(image = image, geometry = "480x340+0+68")
print(r4, info = FALSE)

image_write(r4, path = './outputs/anim_wards_year2.gif')

# Next


image <- image_read('./outputs/ward_map.png')

# Printing the image
print(image, info = FALSE)

image_info(image)

r5<-image_crop(image = image, geometry = "640x471+50+0")
print(r5, info = FALSE)

image_write(r5, path = './outputs/ward_map2.png')
