#Library
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)
library(tibble)

#Basic calculaitons
#All data based on https://ourworldindata.org/population-growth

World_Pop_His <- read_csv("Data/population-and-demography.csv")

#This calculates the total population of the "soviet sphere"
PSS_Pop_1990 <- filter(World_Pop_His, Country  %in% c("Ukraine","Russia", "Belarus", "Latvia", "Estonia", "Lithuania", "Moldova", "Poland", "Albania","Romania","Bulgaria","Hungary","Serbia","Kosovo","Bosnia and Herzegovina","Slovakia","Croatia","slovakia","Slovenia","Czechia","Montenegro","North Macedonia", "Kosovo","Kazakhstan","Uzbekistan","Tajikistan","Kyrgyzstan","Georgia","Azerbaijan","Armenia","Turkmenistan","Mongolia"))%>% 
  select(Country,Year,Population) %>% 
  filter(Year == 1990)%>%
  summarise(TotalPopulation = sum(Population))

#this Calculates the sum of the 6 different countries that Jan Svejnars primary discussion focusses on
Jan_Primary_Pop_1990 <- filter(World_Pop_His, Country  %in% c("Russia", "Poland","Hungary","slovakia","Slovenia","Czechia"))%>% 
  select(Country,Year,Population) %>% 
  filter(Year == 1990)%>%
  summarise(TotalPopulation = sum(Population))

#This calculates the total population of the countries Jan Svejnar made some mentions to, but didnt go in depth with.
Jan_Secondary_pop_1990 <- filter(World_Pop_His, Country  %in% c("Estonia","Latvia","Lithuania","Albania","Bulgaria","Romania","Ukraine","Russia", "Poland","Hungary","slovakia","Slovenia","Czechia"))%>% 
  select(Country,Year,Population) %>% 
  filter(Year == 1990)%>%
  summarise(TotalPopulation = sum(Population))

#Calculating representativity of Jans primary countries
Jan_Primary_Pop_Rep <- Jan_Primary_Pop_1990 %>% 
  mutate(PSS_Pop = PSS_Pop_1990)
Jan_Primary_Pop_Rep <- Jan_Primary_Pop_Rep %>%
  mutate(Representativity = ((TotalPopulation - PSS_Pop$TotalPopulation) / PSS_Pop$TotalPopulation) * 100)

#Calculating representativity of Jans secondary countries
Jan_Secondary_Pop_Rep <- Jan_Secondary_pop_1990 %>% 
  mutate(PSS_Pop = PSS_Pop_1990)
Jan_Secondary_Pop_Rep <- Jan_Secondary_Pop_Rep %>%
  mutate(Representativity = ((TotalPopulation - PSS_Pop$TotalPopulation) / PSS_Pop$TotalPopulation) * 100)

#adding these two calculations to the same DF
Jan_Rep <- Jan_Primary_Pop_Rep %>% 
  rbind(Jan_Secondary_Pop_Rep)
John_Rep_Barchart <- tibble(
  Model= c("Jan Svejnar's Investigation","Jan Svejnar's Total Investigation"),
  Representativity=Jan_Rep$Representativity
)

#plotting these two 
JanRep<- ggplot(John_Rep_Barchart, aes(x = Model, y = Representativity)) +
  geom_bar(stat = "identity", width = 0.3, fill = "darkred") +
  labs(
    title = "Underrepresentation in Jan Svejnars Paper",
    subtitle = "Population of Svejnar's Investigated Countries Measured as\npercentage of total population of 'Post-Soviet Space'",
    caption = "United Nations, World Population Prospects (2022)",
    x = element_blank(),
    y = "Percentage of underrepresentation"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = "black", family = "helvetica"),
    plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic", family = "helvetica"),
    plot.caption = element_text(hjust = 1),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'pink')
  )

JanRep

ggsave("Jan_Svejnar_Underrepresentation.png", width = 8, height = 6, units = "in", dpi = 300)


#now we can try and map it
library(rnaturalearth)

