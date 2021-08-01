
# REQUIRED LIBRARIES

library("gganimate")
library("data.table")
library("knitr")
library("gridExtra")
library("tidyverse")
library("plotly")


# LOAD ATHLETES EVENTS DATA

dataOlympics <- read_csv("datasets/athleteEvents.csv", col_types = cols(
                   ID = col_character(),
                   Name = col_character(),
                   Sex = col_factor(levels = c("M","F")),
                   Age =  col_integer(),
                   Height = col_double(),
                   Weight = col_double(),
                   Team = col_character(),
                   NOC = col_character(),
                   Games = col_character(),
                   Year = col_integer(),
                   Season = col_factor(levels = c("Summer","Winter")),
                   City = col_character(),
                   Sport = col_character(),
                   Event = col_character(),
                   Medal = col_factor(levels = c("Gold","Silver","Bronze"))
                 )
)

glimpse(dataOlympics)
head(dataOlympics)


# LOAD DATA MATCHING NOCs (NATIONAL OLYMPIC COMMITTEE) WITH COUNTRIES

NOCs <- read_csv("datasets/nocRegions.csv", col_types = cols(
                  NOC = col_character(),
                  region = col_character()
                ))
glimpse(NOCs)
head(NOCs)


# NUMBER OF NATIONS, ATHLETES AND AND EVENTS, WITHOUT ART COMPETITIONS

numbers <- dataOlympics %>%
  group_by(Year, Season) %>%
  summarize(Nations = length(unique(NOC)), Athletes = length(unique(ID)), Events = length(unique(Event))
  )

numbers <- numbers %>%
  mutate(gap= if(Year<1920) 1 else if(Year>=1920 & Year<=1936) 2 else 3)

plotNations <- ggplot(numbers, aes(x=Year, y=Nations, group=interaction(Season,gap), color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("chocolate","deepskyblue4")) +
  labs(x = " ", y = "Nations", 
       title="Nations, Athletes and Events", 
       subtitle = "Olympic Games from 1896 to 2016")
  
plotAthletes <- ggplot(numbers, aes(x=Year, y=Athletes, group=interaction(Season,gap), color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("chocolate","deepskyblue4")) +
  xlab("") 

plotEvents <- ggplot(numbers, aes(x=Year, y=Events, group=interaction(Season,gap), color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("chocolate","deepskyblue4")) 
  
grid.arrange( plotNations, plotAthletes, plotEvents, ncol=1)


# THE TOTAL NUMBER OF MEDALS GIVEN TO EACH TEAM

medalCounts <- dataOlympics %>% filter(!is.na(Medal))%>% 
  group_by(NOC, Medal, Event, Games) %>%
  summarize(isMedal=1)

medalCounts <-  medalCounts %>% 
  group_by(NOC, Medal) %>%
  summarize(Count= sum(isMedal))

medalCounts <- left_join(medalCounts, NOCs, by= "NOC" )

medalCounts <- medalCounts %>% 
  mutate (Team = region)

medalCounts <- medalCounts %>% select( Medal, Team, Count)


# ORDERING TEAM BY TOTAL MEDAL COUNT

levelsTeam <- medalCounts %>%
  group_by(Team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(desc(Total)) %>%
  select(Team) %>%
  slice(30:1)

medalCounts$Team <- factor(medalCounts$Team, levels=levelsTeam$Team)

medalCounts <- medalCounts %>% filter(Team != "NA")


# PLOT MEDAL COUNTS

ggplot(medalCounts, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold3","gray64","sienna")) +
  labs(x = "Nations", y = "Count", 
       title="Top 30 - Nations with the most medals won in the history", 
       subtitle = "Olympic Games from 1896 to 2016") 


# NUMBER OF MEDALS GIVEN TO EACH TEAM

medalCounts <- dataOlympics %>% filter(!is.na(Medal))%>% 
  group_by(NOC, Medal, Event, Games, Year) %>%
  summarize(isMedal=1)

medalCounts <-  medalCounts %>% 
  group_by(NOC, Medal, Year) %>%
  summarize(Count= sum(isMedal))

medalCounts <- left_join(medalCounts, NOCs, by= "NOC" )

medalCounts <- medalCounts %>% 
  mutate (Team = region)

medalCounts <- medalCounts %>% select( Medal, Team, Count, Year)


# ORDERING TEAM BY TOTAL MEDAL COUNT

levelsTeam <- medalCounts %>%
  group_by(Team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(desc(Total)) %>%
  select(Team) %>%
  slice(10:1)

medalCounts$Team <- factor(medalCounts$Team, levels=levelsTeam$Team)

medalCounts <- medalCounts %>% filter(Team != "NA")


# ANIMATED PLOT MEDAL COUNT
 
plotMedalsAnim<- ggplot(medalCounts, aes(x=Team, y=Count, fill=Medal)) +
  labs(x = "Nations", y = "Count", 
       title='Top 10 - Comparison over time, nations with the most medals', 
       subtitle = 'Olympic Games from 1896 to 2016 - Year: {frame_time}')  +
  transition_time(Year)+
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold3","gray64","sienna"))
  
animate(plotMedalsAnim,fps=2)


# MAP NATIONS WITH MOST MEDALS WON

medalCounts <- dataOlympics %>% filter(!is.na(Medal))%>% 
  group_by(NOC, Medal, Event, Games) %>%
  summarize(isMedal=1)

medalCounts <-  medalCounts %>% 
  group_by(NOC, Medal) %>%
  summarize(Count= sum(isMedal))

medalCounts <- left_join(medalCounts, NOCs, by= "NOC" ) %>% 
  select(region, NOC, Medal, Count)

medalCounts <- medalCounts %>%
  group_by(region) %>%
  summarize(Total=sum(Count))

data_regions <- medalCounts %>% 
  left_join(NOCs,by="region") %>%
  filter(!is.na(region))

earth <- map_data("world")

earth <- left_join(earth, data_regions, by="region")


# PLOT MAP 

plotMapMedals <- ggplot(earth, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Total, label= region)) +
  labs(x = "", y = "", 
       title="Map of nations with the most medals won", 
       subtitle = "Olympic Games from 1896 to 2016") +
  guides(fill=guide_colourbar(title="medals")) +
  scale_fill_gradient(low="white",high="gold3")
plotMapMedals

ggplotly(plotMapMedals)


# PARTICIPATION OF MALE AND FEMALE ATHLETES OVER TIME, WITHOUT ART COPETITIONS

dataOlympics <- dataOlympics %>% 
  filter(Sport != "Art Competitions")


# AFTER 1992, CHANGE THE YEAR OF THE WINTER GAMES TO COINCIDE WITH THE NEXT SUMMER GAMES. THE TERM "YEAR" CURRENTLY REFERS TO THE OLYMPICS TOOK PLACE

original <- c(1994,1998,2002,2006,2010,2014)

new <- c(1996,2000,2004,2008,2012,2016)

for (i in 1:length(original)) {
  dataOlympics$Year <- gsub(original[i], new[i], dataOlympics$Year)
}

dataOlympics$Year <- as.integer(dataOlympics$Year)


# COUNT NUMBER OF ATHLETES BY SEX AND YEAR

countsSex <- dataOlympics %>% 
  group_by(Year, Sex) %>%
  summarize(Athletes = length(unique(ID)))

countsSex$Year <- as.integer(countsSex$Year)


# PLOT MALE AND FEMALE ATHLETES OVER TIME

ggplot(countsSex, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  transition_reveal(Year)+
  scale_color_manual(values=c("deepskyblue4","red4")) +
  labs(x = "Year", y = "Athletes", 
       title="Male and Female athletes over time", 
       subtitle = "Olympic Games from 1896 to 2016")


