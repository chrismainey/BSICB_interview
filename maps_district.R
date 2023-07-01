library(sp)
# Load library
library(sf)

# Load shapefile

shapename_LA<- st_read("./LAD_Dec_2019_UK_BFE_2022/LAD_DEC_2019_UK_BFE.shp")


map_LA_dt <-
  animal_dt %>% 
  left_join(shapename_LA, c("District" = "LAD19NM"))

shape_WM_LA <- 
  shapename_LA %>% 
  filter(LAD19NM %in% unique(animal_dt$District))


map_LA_agg <- 
  map_LA_dt %>% 
  group_by(District, geometry) %>% 
  summarise(Rescues = n())


# get right shape first
map_LA1<- ggplot() + 
  geom_sf(data = shape_WM_LA, aes(geometry = geometry)
          , size = 1.5
          , color = "black"
          #, fill = "cyan1"
          , 
  ) + 
  
  geom_sf(data = map_LA_agg, aes(geometry = geometry, fill= Rescues)
          , size = 1.5
          , color = "black"
          #, fill = "cyan1"
  ) + 
  geom_sf_text(data = shape_WM_LA, aes(label = LAD19NM), col="white", size=3.5)+
  scale_fill_viridis_c(alpha=0.6, direction = 1)+
  #transition_time(year) +
  labs(title ="Animal Rescuse by District 2013 - 2023"
       , subtitle = "Data Source: https://www.cityobservatory.birmingham.gov.uk/@west-midlands-fire-service/animal-rescues") + 
  coord_sf()+
  theme_map()

map_LA1

# Export
ragg::agg_png("./outputs/LA_map.png" , width = 758, height = 471, units = "px")
map_LA1
dev.off()

       