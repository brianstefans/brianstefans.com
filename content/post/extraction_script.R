library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)



# #Pick a problem
# Build a model that solves it
# Build an api around iterators
# host it
# set up monitoring
# build a retraining pipeline
# write about it

# week7
lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv') 
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

#DuBoisChallenge
georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/census.csv')
furniture <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/furniture.csv')
city_rural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/city_rural.csv')
income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/income.csv')
freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')
occupation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/occupation.csv')
conjugal <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/conjugal.csv')


employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')


zip::unzip("http://openpsychometrics.org/_rawdata/BIG5.zip")

dta <- read.csv("C:/Users/Brian Stephen Ssali/OneDrive/Scripts/Others/Portifolio/brianstefans.github.io/content/post/2021-03-18-analysing-likert-scaled-data/Data/BIG5/BIG5/data.csv",header = T,sep = " ")

read.delim("C:/Users/Brian Stephen Ssali/OneDrive/Scripts/Others/Portifolio/brianstefans.github.io/content/post/2021-03-18-analysing-likert-scaled-data/Data/BIG5/codebook.txt") %>% View()

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

api_id <- "mmk5aymdvobu3edmkpzf90y7"
api_secret <- "3npfetbaw5y774i2tjjnn0w3gisbjwgsfchnsri4uau9roi9bu"
app_token <- "hB0gTGthLgBCODbUqiV8iAKn9"

readr::read_csv(paste0("https://data.waterpointdata.org/resource/jfkt-jmqa.csv?%24limit=50&%24%24","app_token=",api_secret)) %>% View()

## install.packages("RSocrata")

library("RSocrata")

df <- read.socrata("https://data.waterpointdata.org/resource/jfkt-jmqa.json?country_name=Uganda",
                   app_token = app_token,#"YOURAPPTOKENHERE",
                   email     = "brianstefans@gmail.com",
                   password  = "incorrect12."
)                

df %>% readr::write_rds(paste0("D:/GIS/Data/water_sources_ug_",lubridate::today(),".rds"))
water %>% readr::write_rds(paste0("E:/Learning/Tidytuesday/water",lubridate::today(),".rds"))


fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')

fishing %>% 
  filter(str_detect(region, "U.S. Total")) %>% 
  filter(!is.na(region)) %>% 
  mutate(species = forcats::fct_lump(species, 12)) %>% 
  filter(species != "Other") %>% 
  ggplot(aes(x = year, y = values, color = lake)) +
  geom_line() +
  facet_wrap(~species) +
  theme(legend.position = "top")

africa_shp %>% rename(country_name = ADM0_NAME) %>% 
  inner_join(water %>% data.frame() %>% filter(!is.na(lat_deg),!is.na(water_source)) %>% head(100)) %>% 
  ggplot() +
  geom_sf()+
  geom_point(aes(x = lon_deg, y = lat_deg,colour = water_source))+
  facet_wrap(. ~ water_source)

library(leaflet)
library(sf)
africa_shp <- sf::st_read("D:/Maps/Africa shapefile/africa_shapefile.shp")
  
africa_shp %>% rename(country_name = ADM0_NAME) %>% 
  left_join(water_dta %>% as_tibble()%>% filter(!is.na(lat_deg)) %>% head(100)) %>% 
  leaflet() %>% 
  addPolygons() %>% 
  addCircles(~lon_deg,~lat_deg,color = "green")

water %>% filter(!is.na(lat_deg)) %>% head(100) %>% head()

africa_shp %>% rename(country_name = ADM0_NAME) %>% 
  left_join(water_dta %>% count(country_name,water_source) %>% filter(!is.na(water_source),str_detect(water_source,"Protected Spring")) ) %>% 
  #mutate(n = case_when(n > 0~as.integer(0),TRUE~as.integer(0))) %>% #st_drop_geometry() %>% 
  ggplot() +
  geom_sf(aes(fill = n))


africa_shp %>% rename(country_name = ADM0_NAME) %>% 
  left_join(water_dta %>% count(country_name,water_source) %>% filter(!is.na(water_source)) %>% arrange(country_name,desc(n)) %>% distinct(country_name,.keep_all = T) ) %>% 
  #mutate(n = case_when(n > 0~as.integer(0),TRUE~as.integer(0))) %>% #st_drop_geometry() %>% 
  ggplot() +
  geom_sf(aes(fill = water_source))

africa_shp %>% rename(country_name = ADM0_NAME) %>% 
  left_join(water_dta %>% count(country_name,water_source) %>% filter(!is.na(water_source)) %>% arrange(country_name,n) %>% distinct(country_name,.keep_all = T) ) %>% 
  #mutate(n = case_when(n > 0~as.integer(0),TRUE~as.integer(0))) %>% #st_drop_geometry() %>% 
  ggplot() +
  geom_sf(aes(fill = water_source))

CLZ <- c(RColorBrewer::brewer.pal(name = "Dark2",n = 8),RColorBrewer::brewer.pal(name = "Paired",n = 8))
africa_shp %>% rename(country_name = ADM0_NAME) %>% 
  left_join(water_dta %>% count(country_name,water_source) %>% filter(!is.na(water_source)) %>% 
              arrange(country_name,n) %>% distinct(country_name,.keep_all = T) %>% right_join(africa_shp %>% select(country_name = ADM0_NAME) %>% st_drop_geometry() )%>% mutate(cat = "minority") %>% 
              bind_rows(water_dta %>% count(country_name,water_source) %>% filter(!is.na(water_source)) %>%
                          arrange(country_name,desc(n)) %>% distinct(country_name,.keep_all = T)%>% right_join(africa_shp %>% select(country_name = ADM0_NAME) %>% st_drop_geometry() )%>% mutate(cat = "majority")  )) %>%
  #st_drop_geometry() %>% View()
  #mutate(n = case_when(n > 0~as.integer(0),TRUE~as.integer(0))) %>% #st_drop_geometry() %>% 
  ggplot() +
  geom_sf(aes(fill = water_source))+ggthemes::theme_map()+theme(legend.background =element_rect(fill = NA))+
  scale_fill_manual(values = CLZ)+#  viridis::scale_fill_viridis(discrete = T)+# scale_fill_brewer(palette = "Set1")+# theme(axis.text = element_blank(),panel.grid = element_blank(),axis.ticks = element_blank(),axis.line = element_blank())+
  facet_wrap(. ~ cat)

africa_shp %>% rename(country_name = ADM0_NAME) %>% 
  left_join(water_dta %>% count(country_name,water_source) %>% filter(!is.na(water_source)) %>%
              arrange(country_name,desc(n)) %>% distinct(country_name,.keep_all = T)%>% right_join(africa_shp %>% select(country_name = ADM0_NAME) %>% st_drop_geometry() )%>% mutate(cat = "majority") ) %>%
  mutate(water_source = case_when(is.na(water_source)~"No data",TRUE~water_source)) %>% 
  #st_drop_geometry() %>% View()
  #mutate(n = case_when(n > 0~as.integer(0),TRUE~as.integer(0))) %>% #st_drop_geometry() %>% 
  ggplot() +
  geom_sf(aes(fill = water_source))+ggthemes::theme_map()+
  theme(legend.background =element_rect(fill = NA),legend.position = "bottom")+
  scale_fill_manual(values = CLZ)#+#  viridis::scale_fill_viridis(discrete = T)+# scale_fill_brewer(palette = "Set1")+# theme(axis.text = element_blank(),panel.grid = element_blank(),axis.ticks = element_blank(),axis.line = element_blank())+
  facet_wrap(. ~ cat)
            

water_dta %>% count(country_name,water_source) %>% filter(!is.na(water_source)) %>% arrange(country_name,n) %>% View()
  distinct(country_name,.keep_all = T) %>% View()
