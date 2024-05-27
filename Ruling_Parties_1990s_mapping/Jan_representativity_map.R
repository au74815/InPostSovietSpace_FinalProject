library(rnaturalearth)
library(tidyverse)
library(ggplot2)
library(tibble)


#getting the sf files for the different countries
countries <- ne_countries()

#Creating and previewing the countries of the "Post-Soviet Space" or as i abreviate it from now on: "PSS"
Jan_Rep_Map<- filter(countries, sovereignt %in% c("Ukraine","Russia", "Belarus", "Latvia", "Estonia", "Lithuania", "Moldova", "Poland", "Albania","Romania","Bulgaria","Hungary","Republic of Serbia","Kosovo","Bosnia and Herzegovina","Slovakia","Croatia","slovakia","Slovenia","Czechia","Montenegro","North Macedonia", "Kosovo","Kazakhstan","Uzbekistan","Tajikistan","Kyrgyzstan","Georgia","Azerbaijan","Armenia","Turkmenistan","Mongolia"))%>% 
  select(sovereignt,geometry,formal_en)

#Previewing that it works
mapview(Jan_Rep_Map)

#creating a df that denotes whether or not the different countries are mentioned by Jan Svejnar
Jan_fully_investigated <- tibble(
  sovereignt = c("Ukraine","Russia", "Belarus", "Latvia", "Estonia", "Lithuania", "Moldova", "Poland", "Albania","Romania","Bulgaria","Hungary","Republic of Serbia","Kosovo","Bosnia and Herzegovina","Slovakia","Croatia","Slovenia","Czechia","Montenegro","North Macedonia", "Kosovo","Kazakhstan","Uzbekistan","Tajikistan","Kyrgyzstan","Georgia","Azerbaijan","Armenia","Turkmenistan","Mongolia"),
  is_fully_investigated = c("no","yes","no","no","no","no","no","yes","no","no","no","yes","no","no","no","yes","no","yes","yes","no","no","no","no","no","no","no","no","no","no","no","no")
)

#Now im left joining the SF countrydata with the DF i just created
Jan_Full_Rep<- left_join(Jan_Rep_Map, Jan_fully_investigated, by ="sovereignt")


#...And making the map
Jan_Full_Rep_Map <- ggplot(Jan_Full_Rep, aes(fill=is_fully_investigated))+
  scale_fill_manual(values = c("yes" = "pink2", "no" = "darkred"))+
  geom_sf(color = "white",size = 5) +
  coord_sf(xlim = c(11, 125), ylim = c(36, 68), expand = FALSE) +
  labs(title="Bias of Jan Svejnar?",
       subtitle="Is x country of the post-soviet space fully investigated in the paper by Svejnar?",
       caption="Jan Svejnar 2002")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        plot.caption = element_text(hjust = 1,size = 7),
        axis.title.x=element_blank(),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank()
        )

Jan_Full_Rep_Map


#creating a df that denotes whether or not the different countries are fully OR passingly investigated by Jan Svejnar
Jan_fully_OR_partially_investigated <- tibble(
  sovereignt = c("Ukraine","Russia", "Belarus", "Latvia", "Estonia", "Lithuania", "Moldova", "Poland", "Albania","Romania","Bulgaria","Hungary","Republic of Serbia","Kosovo","Bosnia and Herzegovina","Slovakia","Croatia","Slovenia","Czechia","Montenegro","North Macedonia", "Kosovo","Kazakhstan","Uzbekistan","Tajikistan","Kyrgyzstan","Georgia","Azerbaijan","Armenia","Turkmenistan","Mongolia"),
  is_fully_investigated = c("partly","yes","no","partly","partly","partly","no","yes","partly","partly","partly","yes","no","no","no","yes","no","yes","yes","no","no","no","no","no","no","no","no","no","no","no","no")
)

#Now im left joining the SF countrydata with the DF i just created
Jan_Full_OR_Part_Rep<- left_join(Jan_Rep_Map, 
                                     Jan_fully_OR_partially_investigated, 
                                     by ="sovereignt")


#...And making the map
Jan_Full_OR_Part_Rep_Map <- ggplot(Jan_Full_OR_Part_Rep, aes(fill=is_fully_investigated))+
  scale_fill_manual(values = c("yes" = "pink2", "no" = "darkred", "partly" = "pink4"))+
  geom_sf(color = "white",size = 1) + 
  coord_sf(xlim = c(11, 125), ylim = c(36, 68), expand = FALSE) +
  labs(title="Bias of Jan Svejnar?",
       subtitle="Is x country of the post-soviet space investigated at all in the paper by Svejnar?",
       caption="Jan Svejnar 2002")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 8),
        plot.caption = element_text(hjust = 1,size = 7),
        axis.title.x=element_blank(),
        panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank()
  )

Jan_Full_OR_Part_Rep_Map

#now we are utilizing the data acquired in our other R-script; "PSS_GDP_MAP.R"
# we are creating the map to check whether or not jan has perhaps cherrypicked
#some countries for his researcj
Jan_bias_by_GDP1<- left_join(PSS_GDP1990s,
                            Jan_fully_OR_partially_investigated, 
                            by="sovereignt",
                            relationship = "many-to-many") %>% 
  filter(is_fully_investigated=="yes")

#i originally intended to animate this map but i decided against it for clarity and ease of comparison
#this has caused some of the code to be redundant, nonetheless the final product works all the same, it
#may just not be the most elegant solution.
#Here im only showing the last year which will ease comparison:
Jan_bias_by_GDP1 <- Jan_bias_by_GDP1%>% 
  filter(Year == 1999)

#Creating the first unfinished map
Jan_bias_by_GDP_map1 <- ggplot(Jan_bias_by_GDP1, aes(fill=Rel_GDP_Change))+
  geom_sf(color = "white",size = 5) +
  scale_fill_stepsn(colours = c("darkred","red3", "white","#FFE6A5","gold","#FFB700","#FFB700"),
                    breaks = c(-60, -40, -20, -0.1, 20, 40, 60),
                    name=NULL)+
  coord_sf(xlim = c(11, 125), ylim = c(36, 68), expand = FALSE) +
  labs(title="Does Svejnar have bias?",
       subtitle="GDP Change of 'Svejnars fully investigated countries' 1990-99",
       caption="Feenstra et al. (2015), Penn World Table (2021)", "Svejnar")+
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
Jan_bias_by_GDP_map1

#Creating the second part of the map that changes the presentation of it to be in line with the other maps of the project
Jan_bias_by_GDP_map1<- Jan_bias_by_GDP_map1 +
  labs(subtitle = "Relative 1999 GDP change (in percent) of the countries Jan Svejnar fully investigated in his paper (1990=0%)",tag="1999",fill="") +
  geom_sf(color = "white",size = 20)+
  coord_sf(xlim = c(11, 125), ylim = c(35, 68), expand = FALSE) +
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

Jan_bias_by_GDP_map1
ggsave("Jan_bias_by_GDP_map1.jpg", plot = Jan_bias_by_GDP_map1, width = 17.5, height = 9)


#Very nice! Now lets see who Jan Svejnar didn't give as much attention
#We are again utilizing the data acquired in our other R-script; "PSS_GDP_MAP.R"
# we are creating the map to check whether or not Jan has perhaps cherrypicked
#some countries for his research
Jan_bias_by_GDP2<- left_join(PSS_GDP1990s,
                             Jan_fully_OR_partially_investigated, 
                             by="sovereignt",
                             relationship = "many-to-many") %>% 
  filter(is_fully_investigated!="yes")
#Here im filtering away everything but the last year which will ease comparison:
Jan_bias_by_GDP2 <- Jan_bias_by_GDP2%>% 
  filter(Year == 1999)


##preparing the map for finish
Jan_bias_by_GDP_map2 <- ggplot(Jan_bias_by_GDP2, aes(fill=Rel_GDP_Change))+
  geom_sf(color = "white",size = 5) +
  scale_fill_stepsn(colours = c("gray15","darkred","red3", "white","gold"),
                    breaks = c(-60, -40, -20, 0, 20, 40, 60),
                    name=NULL)+
  coord_sf(xlim = c(11, 125), ylim = c(36, 68), expand = FALSE) +
  labs(title="A Svejnar bias to speak of?",
       subtitle="Relative GDP Change of the countries svejnar did not investigate in his paper, 1990-99",
       caption="Feenstra et al. (2015), Penn World Table (2021)", "Svejnar (2002)")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.25,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

Jan_bias_by_GDP_map2<- Jan_bias_by_GDP_map2 +
  labs(subtitle = "Relative 1999 GDP change (in percent) of the countries Jan Svejnar did not fully investigate in his paper (1990=0%)",tag="1999",fill="") +
  geom_sf(color = "white",size = 20)+
  coord_sf(xlim = c(11, 125), ylim = c(35, 68), expand = FALSE) +
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
#it looks a lot nicer saved than in the viewfinder!
Jan_bias_by_GDP_map2
ggsave("Jan_bias_by_GDP_map2.jpg", plot = Jan_bias_by_GDP_map2, width = 17.5, height = 9)

#Hmmm, not so nice! Lets finally compare the countries he "partly" investigated 
#as well as the ones he totally neglected
Jan_bias_by_GDP3<- left_join(PSS_GDP1990s,
                             Jan_fully_OR_partially_investigated, 
                             by="sovereignt",
                             relationship = "many-to-many") %>% 
  filter(is_fully_investigated!="no")
#Here im filtering away everything but the last year which will ease comparison:
Jan_bias_by_GDP3 <- Jan_bias_by_GDP3%>% 
  filter(Year == 1999)


##preparing the map for finish
Jan_bias_by_GDP_map3 <- ggplot(Jan_bias_by_GDP3, aes(fill=Rel_GDP_Change))+
  geom_sf(color = "white",size = 5) +
  scale_fill_stepsn(colours = c("gray15","darkred","red3", "white","#FFE6A5","gold","#FFB700"),
                    breaks = c(-60, -40, -20, 0, 20, 40, 60),
                    name=NULL)+
  coord_sf(xlim = c(11, 125), ylim = c(36, 68), expand = FALSE) +
  labs(title="A Svejnar bias to speak of?",
       subtitle="Relative GDP Change of the countries svejnar did not investigate in his paper, 1990-99",
       caption="Feenstra et al. (2015), Penn World Table (2021)", "Svejnar (2002)")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.25,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#finishing up the presentation
Jan_bias_by_GDP_map3<- Jan_bias_by_GDP_map3 +
  labs(subtitle = "Relative 1999 GDP change (in percent) of the countries Jan Svejnar fully or partly investigated in his paper (1990=0%)",tag="1999",fill="") +
  geom_sf(color = "white",size = 20)+
  coord_sf(xlim = c(11, 125), ylim = c(35, 68), expand = FALSE) +
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
#here the map is
Jan_bias_by_GDP_map3
ggsave("Jan_bias_by_GDP_map3.jpg", plot = Jan_bias_by_GDP_map3, width = 17.5, height = 9)

#Hmmm, well that doesnt look toooooooooooo bad lets take a look at the ones 
#ones he totally neglected
Jan_bias_by_GDP4<- left_join(PSS_GDP1990s,
                             Jan_fully_OR_partially_investigated, 
                             by="sovereignt",
                             relationship = "many-to-many") %>% 
  filter(is_fully_investigated=="no")
#Here im filtering away everything but the last year which will ease comparison:
Jan_bias_by_GDP4 <- Jan_bias_by_GDP4%>% 
  filter(Year == 1999)


##preparing the map for finish
Jan_bias_by_GDP_map4 <- ggplot(Jan_bias_by_GDP4, aes(fill=Rel_GDP_Change))+
  geom_sf(color = "white",size = 5) +
  scale_fill_stepsn(colours = c("gray15","darkred","red3", "white"),
                    breaks = c(-60, -40, -20, 0, 20, 40, 60),
                    name=NULL)+
  coord_sf(xlim = c(11, 125), ylim = c(36, 68), expand = FALSE) +
  labs(title="A Svejnar bias to speak of?",
       subtitle="Relative GDP Change of the countries svejnar did not investigate in his paper, 1990-99",
       caption="Feenstra et al. (2015), Penn World Table (2021)", "Svejnar (2002)")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.5,size = 10),
        plot.caption = element_text(hjust = 1.25,size = 7),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#finishing up the presentation
Jan_bias_by_GDP_map4<- Jan_bias_by_GDP_map4 +
  labs(subtitle = "Relative 1999 GDP change (in percent) of the countries Jan Svejnar wholly neglected in his paper (1990=0%)",tag="1999",fill="") +
  geom_sf(color = "white",size = 20)+
  coord_sf(xlim = c(11, 125), ylim = c(35, 68), expand = FALSE) +
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
#here the map is
Jan_bias_by_GDP_map4
ggsave("Jan_bias_by_GDP_map4.jpg", plot = Jan_bias_by_GDP_map4, width = 17.5, height = 9)





