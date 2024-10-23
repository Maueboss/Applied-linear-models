library(car)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(bookdown)
library(plotly)
library(rstanarm)
#library(rstudioapi) Use when creating the dataset for the first time
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
### Natural disasters ###

df_nd<-read.csv("data/natural-disasters.csv",sep=",",header=TRUE)
df_nd <- df_nd %>%
  select( Year, Country.name, Total.economic.damages.from.disasters, Number.of.total.people.affected.by.disasters, Death.rates.from.disasters) %>%
  rename(Country=Country.name)

df_nd$Year <- as.character(df_nd$Year)
# The total number of people affected is the sum of injured, requiring assistance and homeless
#‘All disasters’ includes all geophysical, meteorological and climate events including earthquakes, volcanic activity, landslides, drought, wildfires, storms, and flooding.

### Sea levels ###
# Non fatto perché non saprei come collegarlo ai vari paesi
# df_sea <- read.csv("data/Change_in_Mean_Sea_Levels.csv",sep=",",header=TRUE)
# df_sea <- df_sea %>%
#   select(Date, Value)

### Carbon Emission ###

df_CO2 <- read.csv("data/CO2 Data.csv",sep=",",header=TRUE) %>%
  as_tibble()%>%
  select(country, year, population, co2, share_global_co2_including_luc, total_ghg, methane, nitrous_oxide, primary_energy_consumption)%>%
  rename(Year=year, Country=country)

df_CO2$Year <- as.character(df_CO2$Year)

world_co2 <- df_CO2 %>%
  group_by(Year) %>% 
  summarize(co2_world = sum(co2, na.rm=TRUE))

world_methane<- df_CO2 %>%
  group_by(Year) %>% 
  summarize(methane_world = sum(methane, na.rm=TRUE))

world_nitrous <- df_CO2 %>%
  group_by(Year) %>% 
  summarize(nitrous_oxide_world = sum(nitrous_oxide, na.rm=TRUE))

df_CO2 <- df_CO2 %>%
  left_join(world_co2, by="Year")

df_CO2 <- df_CO2 %>%
  left_join(world_methane, by="Year")

df_CO2 <- df_CO2 %>%
  left_join(world_nitrous, by="Year")

df_CO2 <- df_CO2 %>% mutate_at(c('methane_world', "nitrous_oxide_world"), ~na_if(., 0))
### Countries ###

# China

df_pr_ <- read.csv("data/China/pr_timeseries_annual_cru_1901-2021_CHN.csv",sep=",",header=FALSE) %>%
  as_tibble()
  
df_pr_ <- df_pr_[-1,-c(3:33)]
  

df_mean_ <- read.csv("data/China/tasmax_timeseries_annual_cru_1901-2021_CHN.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean_ <-df_mean_[ -c(1,2), -c(3:33)]%>%
  left_join(df_pr_, by="V1")%>%
  mutate(Country="China")

df_mean_$V2.y <- as.character(df_mean_$V2.y)


# Greenland

df_pr <- read.csv("data/Greenland/pr_timeseries_annual_cru_1901-2021_GRL.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:5)] 


df_mean <- read.csv("data/Greenland/tasmax_timeseries_annual_cru_1901-2021_GRL.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:5)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Greenland")
  

df_mean_ <- df_mean_ %>%
  union_all(df_mean)

# Algeria

df_pr <- read.csv("data/Algeria/pr_timeseries_annual_cru_1901-2021_DZA.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:50)]

df_mean <- read.csv("data/Algeria/tasmax_timeseries_annual_cru_1901-2021_DZA.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:50)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Algeria")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)


# Brazil

df_pr <- read.csv("data/Brazil/pr_timeseries_annual_cru_1901-2021_BRA.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:33)]

df_mean <- read.csv("data/Brazil/tasmax_timeseries_annual_cru_1901-2021_BRA.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:33)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Brazil")



df_mean_ <- df_mean_ %>%
  union_all(df_mean)

# Congo

df_pr <- read.csv("data/Congo/pr_timeseries_annual_cru_1901-2021_COG.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:14)]

df_mean <- read.csv("data/Congo/tasmax_timeseries_annual_cru_1901-2021_COG.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:14)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Congo")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)


# Emirati Arabi

df_pr <- read.csv("data/Emirati arabi/pr_timeseries_annual_cru_1901-2021_ARE.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:9)]

df_mean <- read.csv("data/Emirati arabi/tasmax_timeseries_annual_cru_1901-2021_ARE.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:9)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="United Arab Emirates")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)


# Italy

df_pr <- read.csv("data/Italia/pr_timeseries_annual_cru_1901-2021_ITA.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:22)]

df_mean <- read.csv("data/Italia/tasmax_timeseries_annual_cru_1901-2021_ITA.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:22)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Italy")

df_mean_ <- df_mean_ %>%
  union_all(df_mean)

# Netherlands

df_pr <- read.csv("data/Netherlands/pr_timeseries_annual_cru_1901-2021_NLD.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:14)] 

df_mean <- read.csv("data/Netherlands/tasmax_timeseries_annual_cru_1901-2021_NLD.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:14)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Netherlands")



df_mean_ <- df_mean_ %>%
  union_all(df_mean)


# United Kingdom 

df_pr <- read.csv("data/UK/pr_timeseries_annual_cru_1901-2021_GBR.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:6)]

df_mean <- read.csv("data/UK/tasmax_timeseries_annual_cru_1901-2021_GBR.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:6)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="United Kingdom")



df_mean_ <- df_mean_ %>%
  union_all(df_mean)

# USA 

df_pr <- read.csv("data/USA/pr_timeseries_annual_cru_1901-2021_USA.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:53)] 

df_mean <- read.csv("data/USA/tasmax_timeseries_annual_cru_1901-2021_USA.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:53)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="United States")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)

# Germany

df_pr <- read.csv("data/Germany/pr_timeseries_annual_cru_1901-2021_DEU.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:18)]

df_mean <- read.csv("data/Germany/tasmax_timeseries_annual_cru_1901-2021_DEU.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:18)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Germany")

df_mean_ <- df_mean_ %>%
  union_all(df_mean)

# India

df_pr <- read.csv("data/India/pr_timeseries_annual_cru_1901-2021_IND.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:46)] 

df_mean <- read.csv("data/India/tasmax_timeseries_annual_cru_1901-2021_IND.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:46)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="India")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)

# Japan

df_pr <- read.csv("data/Japan/pr_timeseries_annual_cru_1901-2021_JPN.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:49)] 

df_mean <- read.csv("data/Japan/tasmax_timeseries_annual_cru_1901-2021_JPN.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:49)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Japan")

df_mean_ <- df_mean_ %>%
  union_all(df_mean)




# Madagascar

df_pr <- read.csv("data/Madagascar/pr_timeseries_annual_cru_1901-2021_MDG.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:24)] 

df_mean <- read.csv("data/Madagascar/tasmax_timeseries_annual_cru_1901-2021_MDG.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:24)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Madagascar")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)


# Pakistan

df_pr <- read.csv("data/Pakistan/pr_timeseries_annual_cru_1901-2021_PAK.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:10)] 

df_mean <- read.csv("data/Pakistan/tasmax_timeseries_annual_cru_1901-2021_PAK.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:10)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Pakistan")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)

# Myanmar

df_pr <- read.csv("data/Myanmar/pr_timeseries_annual_cru_1901-2021_MMR.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:19)] 

df_mean <- read.csv("data/Myanmar/tasmax_timeseries_annual_cru_1901-2021_MMR.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:19)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Myanmar")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)


# Afghanistan

df_pr <- read.csv("data/Afghanistan/pr_timeseries_annual_cru_1901-2021_AFG.csv",sep=",",header=FALSE) %>%
  as_tibble()

df_pr <- df_pr[-c(1,2),-c(3:36)] 

df_mean <- read.csv("data/Afghanistan/tasmax_timeseries_annual_cru_1901-2021_AFG.csv",sep=",",header=FALSE) %>%
  as_tibble()
df_mean <-df_mean[ -c(1,2), -c(3:36)]%>%
  left_join(df_pr, by="V1")%>%
  mutate(Country="Afghanistan")


df_mean_ <- df_mean_ %>%
  union_all(df_mean)



# Final settings

df <- df_mean_%>%
  rename(Year=V1, Temperature=V2.x, Precipitation=V2.y)

df <- left_join(df, df_CO2, by = c("Year" = "Year", "Country" = "Country"))
df <- left_join(df, df_nd, by = c("Year" = "Year", "Country" = "Country"))

df$Year <- as.numeric(df$Year)
df$Temperature <- as.numeric(df$Temperature)
df$Precipitation <- as.numeric(df$Precipitation)
df$Number.of.total.people.affected.by.disasters<-as.numeric(df$Number.of.total.people.affected.by.disasters)

# Calculate the average precipitation for each country using tapply
average_precipitation <- tapply(df$Precipitation, df$Country, mean)

# Create a new variable based on whether each country's precipitation is above or below the average
df$above_below_average <- ifelse(df$Precipitation > average_precipitation[df$Country], "above average", "below average")

df$dummy_above_below_average <- ifelse(df$above_below_average == "above average", 1, 0)



df_2021<- df%>%
  filter(Year == "2021")

rm(df_CO2, df_mean, df_mean_, df_pr_, df_pr, df_nd)

write.csv(df, file = "data/final_df.csv", row.names = FALSE)


