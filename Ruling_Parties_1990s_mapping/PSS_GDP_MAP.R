library(rnaturalearth) #get the Country SF's
library(tidyverse) #needed
library(spdplyr) #needed for sf files
library(RColorBrewer) #needed for colourbrewing
library(dplyr) #essential for code
library(tidyr) #essential for code
library(sf) #useful for sf objects
library(ggmap) #help with mapping
library(gganimate) #create animated maps
library(ggplot2) #create plots and maps
library(gifski) #render animated map
library(ggtext) #change fonts
library(mapview) #easily preview maps

#countrydata from NE packaage
countries <- ne_countries()

#Creating and previewing the countries of the "Post-Soviet Space" or as i abreviate it from now on: "PSS"
PSS<- filter(countries, sovereignt %in% c("Ukraine","Russia", "Belarus", "Latvia", "Estonia", "Lithuania", "Moldova", "Poland", "Albania","Romania","Bulgaria","Hungary","Republic of Serbia","Bosnia and Herzegovina","Slovakia","Croatia","Slovenia","Czechia","Montenegro","North Macedonia", "Kosovo","Kazakhstan","Uzbekistan","Tajikistan","Kyrgyzstan","Georgia","Azerbaijan","Armenia","Turkmenistan","Mongolia"))%>% 
  select(sovereignt,geometry,formal_en)

#previewing that it works
mapview(PSS)

#Acquiring the GDP data this is the (ginourmous) dataset containing historical gdp values
#for most countries in the world
Long_run_gdp <- read_csv("Data/Long_Run_National_GDP.csv")

#Now we add the historical GDP-data for the former "soviet space" by joining the GDP-dataset 
# with the sf-country dataset
PSS_GDP <- left_join(PSS,Long_run_gdp,by="sovereignt")
mapview(PSS_GDP, z="GDP (output, multiple price benchmarks)", 
        col.regions=brewer.pal(9, "RdYlGn"),
        color="black",
        alpha.regions=1,layer.name="1999 GDP (1990=100)")

#Serbia was refusing to join due to a minor error in the dataset, ne_countries,
#that changed its name to be inconsistent with the others so i had to
#change it to be consistent with the gdp data (and itself)
#this is had to do manually


#The map now works, however Kosovo does not have GDP-data:/
mapview(PSS_GDP, z="GDP (output, multiple price benchmarks)", 
        col.regions=brewer.pal(9, "RdYlGn"),
        color="black",
        alpha.regions=1,layer.name="1999 GDP (1990=100)")

#As kosovo gained their independence in 2008 i will attempt to simply add Serbias data to Kosovo
serbia_rows <- Long_run_gdp$sovereignt == "Republic of Serbia"
kosovo_data <- Long_run_gdp[serbia_rows, ]
kosovo_data$sovereignt <- "Kosovo"
Long_run_gdp <- rbind(Long_run_gdp, kosovo_data)

#After correcting this we will try again
PSS_GDP <- left_join(PSS,Long_run_gdp,by="sovereignt")

#It works!
mapview(PSS_GDP, z="GDP (output, multiple price benchmarks)", 
        col.regions=brewer.pal(9, "RdYlGn"),
        color="black",
        alpha.regions=1,layer.name="1999 GDP (1990=100)")

#hmm, it seems that Armenia, Bosnia, Kyrgyzstan, Tajikistan and Turkmenistan are missing data for 1990.
#by taking a closer look at the dataset we are confronted with something a bit unfortunate
#while Armenia and Tajikistan both have data starting in 1992, Kyrgyzstan starts only in 
#1995 and Turkmenistan not before 1999. Tajikistan also has a missing datapoint for 1997. 
#Meanwhile Bosnia does have data already from 1993, but that data seems highly anomalous. 
#According to it, Bosnias GDP rose more than 400% from 1993-1999 - this is likely explained 
#by a preceding collapse not shown in the dataset, that (hypothetical) collapse being the 
#result of the Yugoslav war. Additional evidence forsuch a collapse taking place also does 
#present itself in that: 1) Bosnia in 1993 had a GDP aproximately 8,5 times smaller than 
#their former compatriots in Croatia despite having similar populations, #2) Bosnias neighbors 
#(and former compatriots) in Croatia and Serbia both saw disastrous economic collapses in the 
#period 1990-1993, their economies shrinking -35,9% and -56,25% respectively. both of these 
#economies also started shakily recovering after 1993, in line with the bosnian data.
view(Long_run_gdp)

#In any case i deem the data for Armenia, Tajikistan and Kyrgyzstan to be usable. Meanwhile i judge
#that the one datapoint for Turkmenistan is not, i also judge that to be the case for Bosnia due to its
#anomalous nature. We will salvage the data for Armenia, Tajikistan and Kyrgyzstan by assuming retroactive
#standstill which, in light of all the other datapoints, does not seem to be all that plausible, and the
#the effect will just be that the collapse of the Armenian, Tajik and Kyrgyzstan economies will be significantly
#underrepresented in the map.
#1: Salvaging Armenia Data:
Arm_1990 <- PSS_GDP[202, ]
Arm_1990$Year <- 1990
Arm_1991 <- PSS_GDP[202, ]
Arm_1991$Year <- 1991
#2: Salvaging Tajikistan Data:
Taj_1990 <- PSS_GDP[132, ]
Taj_1990$Year <- 1990
Taj_1991 <- PSS_GDP[132, ]
Taj_1991$Year <- 1991
Taj_1997 <- PSS_GDP[136, ]
Taj_1997$Year <- 1997
#3: Salvaging Kyrgyzstan Data:
Kyr_1990 <- PSS_GDP[159, ]
Kyr_1990$Year <- 1990
Kyr_1991 <- PSS_GDP[159, ]
Kyr_1991$Year <- 1991
Kyr_1992 <- PSS_GDP[159, ]
Kyr_1992$Year <- 1992
Kyr_1993 <- PSS_GDP[159, ]
Kyr_1993$Year <- 1993
Kyr_1994 <- PSS_GDP[159, ]
Kyr_1994$Year <- 1994

#Bulgaria and Azerbaijan was also missing two years of date which, if not corrected, will 
#cause some quirks, so im now adding missing data for Bulgaria 1991 and 1992 assuming 
#standstill:
#1: Bulgaria
Bul_1991 <- PSS_GDP[589, ]
Bul_1991$Year <- 1991
Bul_1992 <- PSS_GDP[589, ]
Bul_1992$Year <- 1992

#2: Azerbaijan
Aze_1991 <- PSS_GDP[696, ]
Aze_1991$Year <- 1991
Aze_1992 <- PSS_GDP[696, ]
Aze_1992$Year <- 1992

#Here im binding the new data with the rest of the dataset
PSS_GDP <- rbind(PSS_GDP,Bul_1991,Bul_1992, Aze_1991, Aze_1992, Kyr_1990,Kyr_1991,Kyr_1992,Kyr_1993,Kyr_1994,Taj_1990,Taj_1991,Taj_1997,Arm_1990,Arm_1991)

#I later found the Maddison Project Dataset, which had GDP-figures, for the missing countries.
#So i will be substituting the GDP estimates of the countries missing substantial data, with the new data from the Maddison project, 
#i am aware that mixing GDP estimates can be a methodological problem, but as long as i am concerned here with GDP in relative, and not absolute terms, i believe that this will not be as much of an issue, given that i replace all of the data for the countries missing data before, with data im now getting from the Maddison project.
MaddisonGDP <- read_csv("Data/gdp-maddison-project-database.csv")
MaddisonGDP <- filter(MaddisonGDP, Entity %in% c("Turkmenistan","Armenia","Tajikistan","Kyrgyzstan","Bosnia and Herzegovina"))
colnames(MaddisonGDP) <- c("sovereignt","Code","Year","GDP (output, multiple price benchmarks)","900795-annotations")
MaddisonGDP <- MaddisonGDP%>% 
  filter(Year < 2000) %>% 
  filter(Year > 1989)
MaddisonGDP <- select(MaddisonGDP,sovereignt,Year,"GDP (output, multiple price benchmarks)")
MaddisonGDP <- left_join(PSS,MaddisonGDP,by="sovereignt")
#Removing original Bosnia, Armenia, Tajikistan, Kyrgyzstan and Turkmenistan data:
PSS_GDP<- filter(PSS_GDP, sovereignt %in% c("Ukraine","Russia", "Belarus", "Latvia", "Estonia", "Lithuania", "Moldova", "Poland", "Albania","Romania","Bulgaria","Hungary","Republic of Serbia","Kosovo","Slovakia","Croatia","slovakia","Slovenia","Czechia","Montenegro","North Macedonia", "Kosovo","Kazakhstan","Uzbekistan","Georgia","Azerbaijan","Mongolia"))%>% 
select(sovereignt,geometry,formal_en,Year,"GDP (output, multiple price benchmarks)")
PSS_GDP <- rbind(PSS_GDP,MaddisonGDP)

#Before we can animate this map properly we need to isolate only the years of the 1990's, for both datasets
PSS_GDP1990s <- PSS_GDP %>% 
  filter(Year < 2000) %>% 
  filter(Year > 1989)

#...And calculate the relative GDP change:
PSS_GDP1990s <- PSS_GDP1990s %>%
  group_by(sovereignt) %>% 
  mutate(Rel_GDP_Change=((`GDP (output, multiple price benchmarks)`/`GDP (output, multiple price benchmarks)`[Year==1990])*100)-100)

#Here the GDP developments can be seen plotted, however due to sheer amount of countries being 
#investigated, this is not very readable. We should map it instead.
ggplot(PSS_GDP1990s)+
  geom_line(aes(x=Year,y=Rel_GDP_Change,color=sovereignt))

#The map now works, but i notice that it picks the lowest 'rowvalue' in each group, so the ledger is 
#wrong
mapview(PSS_GDP1990s, z="Rel_GDP_Change", 
        col.regions=brewer.pal(9, "RdYlGn"),
        color="black",
        alpha.regions=1,layer.name="1999 GDP (1990=100)")

#Let us try to create a correct map of the relative GDP change 1990 to 1999
PSS_relGDP1999 <- filter(PSS_GDP1990s, Year==1999)

#mapping with ggplot
PSS_relGDP1999_map<- ggplot() + geom_sf(data = PSS_relGDP1999,
                                     aes(fill=Rel_GDP_Change)) +
  scale_fill_viridis_c()+
  labs(title="Collapse and Growth",
       subtitle="Eastern European GDP Change 1990-99",
       fill="Rel. Change",
       caption="Feenstra et al. (2015), Penn World Table (2021)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.48,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_bar(stat = "identity", position = "dodge")

#Here the map is!
PSS_relGDP1999_map

#However it seems that we should move the it a bit, as that small part
#of Siberia sticking out on the left is forcing the map to zoom out.
#here are other versions with different zoom levels.
#1: Zoom1 - Quite zoomed in! Eastern Europe becomes more legible at the 
#expense of russia looking weird.
PSS_relGDP1999_map_zoom1<- ggplot() + geom_sf(data = PSS_relGDP1999,
                                        aes(fill=Rel_GDP_Change)) +
  scale_fill_viridis_c()+
  labs(title="Collapse and Growth",
       subtitle="GDP Change of the 'Post-Soviet Space' 1990-99",
       fill="Rel. Change",
       caption="Feenstra et al. (2015), Penn World Table (2021)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.48,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_bar(stat = "identity", position = "dodge")+
  coord_sf(xlim = c(11, 121.5), ylim = c(36, 61), expand = FALSE)

PSS_relGDP1999_map_zoom1

#2: Zoom2 - Now russia looks like itself, but Eastern Europe is less legible.
PSS_relGDP1999_map_zoom2<- ggplot() + geom_sf(data = PSS_relGDP1999,
                                              aes(fill=Rel_GDP_Change)) +
  scale_fill_viridis_c()+
  labs(title="Collapse and Growth",
       subtitle="GDP Change of the 'Post-Soviet Space' 1990-99",
       fill="Rel. Change",
       caption="Feenstra et al. (2015), Penn World Table (2021)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.48,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_bar(stat = "identity", position = "dodge")+
  coord_sf(xlim = c(11, 181), ylim = c(36, 81), expand = FALSE)

PSS_relGDP1999_map_zoom2

#3: Zoom3: Here is a third one that is a compromise between the two:
PSS_relGDP1999_map_zoom3<- ggplot() + geom_sf(data = PSS_relGDP1999,
                                        aes(fill=Rel_GDP_Change)) +
  scale_fill_stepsn(colours = c("gray15","darkred","red3", "white","#FFE6A5","gold","#FFB700"),
                    breaks = c(-60, -40, -20, 0, 20, 40, 60),
                    labels = c("-60","-40", "-20", "0", "20","40", "60"),
                    name=NULL)+
  labs(title="Collapse and Growth?",
       subtitle="GDP Change of the 'post-Soviet space' 1990-99",
       fill="Rel. Change",
       caption="Feenstra et al. (2015), Penn World Table (2021), Maddison Project (2024)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.48,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_bar(stat = "identity", position = "dodge")+
  coord_sf(xlim = c(11, 125), ylim = c(34, 68), expand = FALSE)

PSS_relGDP1999_map_zoom3

#While this for sure is alot more legible than our previous graph, it does not show us the data 
#for the years 1991-1999, which is quite unfortunate. One way to rectify this is to *animate* 
#the map.

###ANIMATION
#this script will prepare us to animate it
PSS_relGDP1990s_map <- ggplot(PSS_GDP1990s, aes(fill=Rel_GDP_Change))+
  geom_sf(color = "white",size = 10) +
  scale_fill_stepsn(colours = c("gray15","darkred","red3", "white","#FFE6A5","gold","#FFB700"),
                    breaks = c(-60, -40, -20, -0.001, 20, 40, 60),
                    labels = c("-60","-40", "-20", "0", "20","40", "60"),
                       name=NULL)+
  coord_sf(xlim = c(11, 125), ylim = c(36, 68), expand = FALSE) +
  labs(title="A Post-Soviet Saudade?",
       subtitle="GDP Change of the 'Post-Soviet space' (in%) 1990-99, 1900=0%",
       caption="Feenstra et al. (2015), Penn World Table (2021), Maddison Project (2024)")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.25,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#run this to view the static image of 1990-1999 pregression
PSS_relGDP1990s_map

#run this script to actually animate it
PSS_relGDP1990s_map + transition_states(Year, transition_length=1, state_length=1) +
  labs(subtitle = "Relative GDP Change of 'Post-Soviet Space' since 1990",tag="{closest_state}") +
  exit_fade(alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5,size = 26, face = "bold", color = "black", family="times new roman"),
        plot.subtitle = element_text(hjust = 0.5,size = 10, family="times new roman"),
        plot.caption = element_text(hjust = 1.25,size = 7, family="times new roman"),
        axis.title.x= element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.tag.position = c(0.1, 0.8),
        plot.tag=element_text(size = 26, family="times new roman")
  )

#Now this is an interesting bug! I tried loking at the data to see if anything 
#looked weird, and well it didn't, so im just gonna work my way around this by
#removing the transition, which does make the map animation look slightly less
#appealing but at the same time perhaps it also help clarifty the changes going
#on.
PSS_relGDP1990s_animap<- PSS_relGDP1990s_map + 
  labs(title="In The Post-Soviet Space, No One Can Hear You Scream.", subtitle = "Relative GDP Change of the 'post-Soviet space' 1990-1999",tag="{closest_state}",fill="") +
  transition_states(Year, transition_length=0.0000000001, state_length=0.1) +
  geom_sf(color = "white", size = 8)+
  coord_sf(xlim = c(11, 125), ylim = c(34.7, 67.5), expand = FALSE) +
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
        legend.text = element_text(color = "white",size=11,family="helvetica"),
        legend.direction = "horizontal",
        legend.ticks = element_blank(),
        legend.text.position = "top"
        )

#Now i run this script to animate it. Note that i had to crank the framerate 
#way up cause the tag showing the number apparently was refusing to change in
#sync with the changes on the map, this does bloat the file-size and processing
#time slightly, but then again, these are not very big files.
#PssGDPmap1:
animate(PSS_relGDP1990s_animap, renderer = gganimate::gifski_renderer(), nframes = 40, fps = 1, width = 1080, height = 600, detail=25)
#PssGDPmap1(fast):
animate(PSS_relGDP1990s_animap, renderer = gganimate::gifski_renderer(), nframes = 40, fps = 4, width = 1080, height = 600, detail=25)
#note that i downloaded these files by manually dragging and dropping them from 
#the viewer into my "downloads" folder and from there into the project directory.
