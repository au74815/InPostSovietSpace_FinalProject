library(rnaturalearth)
library(tidyverse)
library(ggplot2)
library(tibble)


#getting the sf files for the different countries
countries <- ne_countries()

#Creating and previewing the countries of the "Post-Soviet Space" or as i abreviate it from now on: "PSS"
Yugoslavia_map<- filter(countries, sovereignt %in% c("Republic of Serbia","Kosovo","Bosnia and Herzegovina","Croatia","Slovenia","Montenegro","North Macedonia"))%>% 
  select(sovereignt,geometry,formal_en)

#Previewing that it works
mapview(Yugoslavia_map)

#now we are utilizing the maddison data acquired in our other R-script; 
#"PSS_GDP_MAP.R" to fetch the Yugoslav GDP-data
Yugo_GDP <- read_csv("Data/gdp-maddison-project-database.csv")
Yugo_GDP <- filter(Yugo_GDP, Entity %in% c("Republic of Serbia","Kosovo","Bosnia and Herzegovina","Croatia","Slovenia","Montenegro","North Macedonia"))
colnames(Yugo_GDP) <- c("sovereignt","Code","Year","GDP (output, multiple price benchmarks)","900795-annotations")
Yugo_GDP <- Yugo_GDP%>% 
  filter(Year < 2000) %>% 
  filter(Year > 1988)
Yugo_GDP <- select(Yugo_GDP,sovereignt,Year,"GDP (output, multiple price benchmarks)")

#Serbia was refusing to join due to a minor error in the dataset, ne_countries
#data set that changed its name to be inconsistent with the others so i had to
#change the maddison project for it to be consistent with ne_countries data
#this is had to do manually.

#...Aditionally, as kosovo gained their independence in 2008 i will attempt to 
#simply add Serbias data to Kosovo
serbia_rows <- subset(Yugo_GDP, sovereignt=="Republic of Serbia")
kosovo_data <- serbia_rows
kosovo_data$sovereignt <- "Kosovo"
Yugo_GDP <- rbind(Yugo_GDP, kosovo_data)


#now it should work
Yugo_GDP <- left_join(Yugoslavia_map,Yugo_GDP,by="sovereignt")


#We have to calculate the relative GDP change:
Yugo_GDP <- Yugo_GDP %>%
  group_by(sovereignt) %>% 
  mutate(Rel_GDP_Change=((`GDP (output, multiple price benchmarks)`/`GDP (output, multiple price benchmarks)`[Year==1989])*100)-100)
#Yugo_GDP <- Yugo_GDP%>% 
  #filter(Year == 1989)

#Creating the first unfinished map
Yugo_GDP1 <- ggplot(Yugo_GDP, aes(fill=Rel_GDP_Change))+
  geom_sf(color = "white",size = 5) +
  scale_fill_stepsn(colours = c("grey15","darkred","red3", "white"),
                    breaks = c(-50, -40, -30, -20, -10, 0, 20, 40, 60),
                    name=NULL)+
  coord_sf(xlim = c(11, 125), ylim = c(36, 68), expand = FALSE) +
  labs(title="Appendix: Yugoslavia with 1989 as base?",
       subtitle="GDP Change of 'Svejnars fully investigated countries' 1990-99",
       caption="Maddison Project (2024)")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.25,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.text = element_text(size = 7),
        axis.ticks.y=element_blank())

#run this to view the static image of 1990-1999 pregression
Yugo_GDP1

#Creating the second part of the map that changes the presentation of 
#it to be in line with the other maps of the project, im changing the map 
#zoom too.
Yugo_GDP1<- Yugo_GDP1 +
  labs(subtitle = "Relative 1999 GDP change (in percent) of the former Yugoslav countries (1989=0%)",tag="1999",fill="") +
  geom_sf(color = "white",size = 20)+
  coord_sf(xlim = c(11.4, 27), ylim = c(40.3, 47.2), expand = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,size = 38, face = "bold", color = "black", family="helvetica"),
        plot.subtitle = element_text(hjust = 0.5,size = 14, face = "italic", family="helvetica"),
        plot.caption = element_text(hjust = 0.5,size = 12, face = "italic", family="helvetica"),
        axis.title.x= element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'black'),
        plot.tag.position = c(0.07, 0.8),
        plot.tag=element_text(size = 45, color = "white", family="helvetica"),
        legend.key.size = unit(1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.position = c(0.86, 0.1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white",size=12,family="helvetica"),
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.text.position = "top"
  )

Yugo_GDP1
ggsave("Yugo_GDP_Map1.jpg", plot = Yugo_GDP1, width = 17.5, height = 9)
