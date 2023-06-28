library(ragg)
library(tidyverse)
library(readxl)
library(fable)
library(feasts)
library(tsibble)

# Set up
showtext::showtext_auto()
sysfonts::font_add_google("Open Sans", "Open Sans")

# ggplot defaults
theme_set(
  theme_classic(base_family = "Open Sans") +
    theme(
      axis.title = element_text(family="Open Sans"),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour ="#425563"),
      strip.background = element_rect(fill = "#c8cfd3"),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(face = "italic", size = 10)
    )
)




url <- "https://birmingham-city-observatory.datopian.com/dataset/e39e6606-4720-4cd8-8b7e-dcc7ca4b44b0/resource/c0e97947-e0f2-4099-98c0-3aeb016b88a7/download/animal-rescue-info-wmids.xlsx"
destfile <- "animal_rescue_info_wmids.xlsx"
curl::curl_download(url, destfile)

animal_dt <- read_excel(destfile
                       , col_types = c("date", "text", "text", "text")
                       , skip = 2
                       , .name_repair = "universal")

# like to view it first
View(animal_dt)


# Initial thoughts, by date, morning/evening, location, type of animal in text.

min(animal_dt$dt)
max(animal_dt$dt)

# Dates look like fiscal years 2013/14 - 2022/23 - 9 years?

# Also add date format as it's easier for tidyverse

animal_dt$dt <- as.Date(animal_dt$Incdate)


##### Consider as timeseries ####

animal_ts <-
  animal_dt %>% 
  mutate(dt_month = yearmonth(dt)) %>% 
  #select(dt_month, District, Ward) %>% 
  group_by(dt_month, District) %>% 
  summarise(Rescues = n()) %>% 
  as_tsibble(index = dt_month, key = c(District))


autoplot(animal_ts)

# check for missing months
has_gaps(animal_ts)
scan_gaps(animal_ts)

# A lot of months at districts
animal_wm_ts <-
  animal_dt %>% 
  mutate(dt_month = yearmonth(dt)) %>% 
  #select(dt_month, District, Ward) %>% 
  group_by(dt_month) %>% 
  summarise(Rescues = n()) %>% 
  as_tsibble(index = dt_month)

# check for missing months
has_gaps(animal_wm_ts)
scan_gaps(animal_wm_ts)

# 3 gaps, median impute

animal_wm_ts <-
  animal_wm_ts %>% 
  fill_gaps(Rescues = median(Rescues))
has_gaps(animal_wm_ts)


# seasonality
animal_wm_ts %>% 
  gg_subseries(Rescues)


# Check autocorrelation
animal_wm_ts %>%  ACF() %>%  autoplot()
animal_wm_ts %>%  ACF(difference(Rescues)) %>%  autoplot()


# decompose the timeseries
dcmp <- animal_wm_ts %>% 
  model(stl = STL(Rescues))

components(dcmp)

components(dcmp) %>%  autoplot()


# Visulaise effects of rolling average
animal_wm_ts <- animal_wm_ts %>% 
  mutate(
    `4-MA` = slider::slide_dbl(Rescues, mean,
                               .before = 4, .after = 0, .complete = TRUE)
  )




animal_wm_ts %>% 
  autoplot(Rescues) +
  geom_line(aes(y = `4-MA`), col="red") +
  labs(y = "Rescues") +
  guides(colour = guide_legend(title = "series"))


# Apply models candidates

mods1 <-
  animal_wm_ts %>% 
  #filter(Date >= yearmonth("2021 Apr")) %>% 
  #stretch_tsibble(.init = 10) %>% 
  model(
    mean = MEAN(Rescues),
    naive = NAIVE(Rescues),
    snaive = SNAIVE(Rescues ~ lag("year")),
    drift = RW(Rescues ~ drift()),
    ets = ETS(Rescues),
    ses = ETS(Rescues ~ error("A")+trend("N")+season("N")),
    `Holt-Winters additive` = ETS(Rescues ~ error("A")+trend("A")+season("A")),
    `Holt-Winters additive damped` = ETS(Rescues ~ error("A")+trend("Ad")+season("A")),
    arima = ARIMA(Rescues)
  )

mods2 <-
  animal_wm_ts %>% 
  filter(dt_month >= yearmonth("2013 Aug")) %>% 
  #stretch_tsibble(.init = 10) %>% 
  model(
    mean = MEAN(`4-MA`),
    naive = NAIVE(`4-MA`),
    snaive = SNAIVE(`4-MA` ~ lag("year")),
    drift = RW(`4-MA` ~ drift()),
    ets = ETS(`4-MA`),
    ses = ETS(`4-MA` ~ error("A")+trend("N")+season("N")),
    `Holt-Winters additive` = ETS(`4-MA` ~ error("A")+trend("A")+season("A")),
    `Holt-Winters additive damped` = ETS(`4-MA` ~ error("A")+trend("Ad")+season("A")),
    arima = ARIMA(`4-MA`)
  )


mnth_forecast <-
  mods1 %>% 
  forecast(h="36 months") 

rolling_forecast <-
  mods2 %>% 
  forecast(h="36 months") 


accuracy(mods1)
accuracy(mods2)

mods2$ets


animal_rf <-
  rolling_forecast %>% 
  hilo(95) %>% 
  filter(.model %in% c("Holt-Winters additive", "arima")) %>% 
  mutate(portion = "Forecast") 


animal_rf$lcl <-animal_rf$`95%`$lower
animal_rf$ucl <- animal_rf$`95%`$upper

animal_rf <-
  animal_rf %>% 
  select(.model, dt_month, .model, `4-MA`, .mean, lcl, ucl)

aminal_rolling_plot <-
  ggplot(animal_wm_ts, aes(x= as.Date(dt_month)))+
  geom_line(aes(y=Rescues), linewidth=1)+
  geom_line(aes(y=.mean, col=.model), data=animal_rf, linewidth=1.2, alpha=0.6)+
  geom_smooth(aes(y=.mean, col=.model), method="lm", data=animal_rf, linewidth=1
              , se=FALSE, linetype="dashed", alpha=0.6)+
  geom_vline(xintercept = as.Date("01/04/2023", format = "%d/%m/%Y"), col = "red"
             , linewidth = 1, linetype="dashed")+
  #geom_ribbon(data=tno_hw, aes(ymin = lcl, ymax=ucl, x= Date, fill = Trust)
  #            , alpha=0.5)+
  #scale_y_continuous(breaks = seq(0,1000,200))+
  scale_x_date("Date"
               , date_breaks = "4 month"
               , date_labels = "%b-%y"
               # , limits = c(as.Date("01/08/2020", format = "%d/%m/%Y"),
               #             as.Date("01/03/2028", format = "%d/%m/%Y"))
               ,expand = c(0,0)
               , date_minor_breaks =  "4 month"
               
  ) +
  scale_color_manual("Model", values=c("#d8b365","#5ab4ac"))+
  labs(title = "Animal Rescues - West Midlands Region"
       , subtitle = "Forecast computed by 'both ARIMA and Holt-Winters (additive) models', based on Apr-13 - Mar-23")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90, size = 8, colour = "#595959"
                                   , hjust = 1, ),
        plot.subtitle = element_text(face="italic", size = 9)
  )
# 

aminal_rolling_plot





#### Distributions

ggplot(animal_dt, aes(x=dt))+
  geom_histogram(fill = "dodgerblue2", col = "black", alpha=0.5)+
  #stat_summary(fun.y=mean,geom="line",lwd=2,aes(group=1))+
  scale_x_date(limits = c(as.Date("2013-04-01", format = "%Y-%M-%D"), as.Date("2023-04-01", format = "%Y-%M-%D"))
               ,date_breaks = "4 month", date_labels = "%Y-%m")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







# 








# Tidytext tokeise most common words for pets