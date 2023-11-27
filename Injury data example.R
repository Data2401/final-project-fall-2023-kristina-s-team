title: "An Extensive Analysis of NFL Injuries"
output:
  html_document:
  code_folding: hide
theme: journal
highlight: tango
df_print: paged
fig_height: 8
fig_width: 11
number_sections: no
toc: yes
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Introduction

This analysis piece will aim to visualise some key relationships associated with National Football League (NFL) injuries.

The analysis will then be used to create suggestions on the steps that should be taken in an attempt to minimise the risk of player injuries. This is of key importance, as the narrative around the NFL for some years now has been centered on the violent nature of the game and parents experiencing varying levels of anxiety when deciding whether to let their children play the sport.

The NFL has tried to get on the front foot; since 2002 alone, the NFL has made 50 rules changes intended to eliminate potentially dangerous tactics and reduce the risk of injuries. They have been proactive in engaging the analytics community, and this project yet another example of this.


*I will be building on this analysis over the coming week, so hopefully you continue to check back in periodically. An upvote would be appreciated if you like the analysis, as would suggestions in the comments.*
  
  ## The Goal
  
  In this challenge, competitors have been tasked to investigate the relationship between the playing surface and the injury and performance of NFL athletes and to examine factors that may contribute to lower extremity injuries, specifically, to help them examine the effects that playing on *synthetic turf* versus *natural turf* can have on player movements and the factors that may contribute to lower extremity injuries.


```{r, message=FALSE, warning=FALSE}
# libraries
library(tidyverse)
library(lubridate)
library(scales)
library(ggridges)

# set up plotting theme
theme_jason <- function(legend_pos="top", base_size=12, font=NA){
  
  # come up with some default text details
  txt <- element_text(size = base_size+3, colour = "black", face = "plain")
  bold_txt <- element_text(size = base_size+3, colour = "black", face = "bold")
  
  # use the theme_minimal() theme as a baseline
  theme_minimal(base_size = base_size, base_family = font)+
    theme(text = txt,
          # axis title and text
          axis.title.x = element_text(size = 15, hjust = 1),
          axis.title.y = element_text(size = 15, hjust = 1),
          # gridlines on plot
          panel.grid.major = element_line(linetype = 2),
          panel.grid.minor = element_line(linetype = 2),
          # title and subtitle text
          plot.title = element_text(size = 18, colour = "grey25", face = "bold"),
          plot.subtitle = element_text(size = 16, colour = "grey44"),
          
          ###### clean up!
          legend.key = element_blank(),
          # the strip.* arguments are for faceted plots
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 13, colour = "grey35")) +
    
    #----- AXIS -----#
    theme(
      #### remove Tick marks
      axis.ticks=element_blank(),
      
      ### legend depends on argument in function and no title
      legend.position = legend_pos,
      legend.title = element_blank(),
      legend.background = element_rect(fill = NULL, size = 0.5,linetype = 2)
      
    )
}


plot_cols <- c("#498972", "#3E8193", "#BC6E2E", "#A09D3C", "#E06E77", "#7589BC", "#A57BAF", "#4D4D4D")

```


## The Data

There are three files provided in the dataset, as described below:
  
  * Injury Record: The injury record file in .csv format contains information on 105 lower-limb injuries that occurred during regular season games over the two seasons. Injuries can be linked to specific records in a player history using the PlayerKey, GameID, and PlayKey fields.
* Play List: – The play list file contains the details for the 267,005 player-plays that make up the dataset. Each play is indexed by PlayerKey, GameID, and PlayKey fields. Details about the game and play include the player’s assigned roster position, stadium type, field type, weather, play type, position for the play, and position group.
* Player Track Data: player level data that describes the location, orientation, speed, and direction of each player during a play recorded at 10 Hz (i.e. 10 observations recorded per second).

```{r}
injury_record <- data.table::fread("../input/nfl-playing-surface-analytics/InjuryRecord.csv", stringsAsFactors = F)
player_tracking <- data.table::fread("../input/nfl-playing-surface-analytics/PlayerTrackData.csv", stringsAsFactors = F)
play_list <- data.table::fread("../input/nfl-playing-surface-analytics/PlayList.csv", stringsAsFactors = F)

```

**Target Variable Observations**
  
  Interestingly, there are only 105 observations in the Injury Record file, and these injuries are only limited to lower-limb injuries. This will make our job a little more interesting. Additionally, of the 105 recorded injuries, 28 do not have a `play_key` recorded - I'm assuming these injuries occurred in a game, but the exact play they occurred is not know. This leaves us with 77 injuries that player tracking data might be used, unless other inferences can be made.

***

# Data Cleaning

This section will deal with cleaning up some of the variables that appear to be a bit of a mess. Some of these variables were also an issue in the NFL Big Data Bowl competition, of which my EDA there dealt with some of these, and can be found [here](https://www.kaggle.com/jaseziv83/comprehensive-cleaning-and-eda-of-all-variables).

### Stadium Type

There are `r length(unique(play_list$StadiumType))` different stadium types in the `play_list` dataset, which should immediately sound of some alarm bells.

When we inspect these levels, we can see that there are minor differences in the description of these values, in addition to spelling errors.

```{r, warning=FALSE, message=FALSE}
play_list %>% 
  count(StadiumType) %>% 
  rename(Count = n) %>% 
  mutate(Count = comma(Count)) %>% 
  kableExtra::kable(format = "html", escape = F) %>%
  kableExtra::kable_styling("striped", full_width = F) %>% 
  kableExtra::scroll_box(height = "500px") %>%
  kableExtra::kable_styling(fixed_thead = T)
```


We'll clean these up to five distinct types, and also an additional one for *unknown*:
  
  * `outdoor`
* `indoor_closed`
* `indoor_open`
* `dome_closed`
* `dome_open`

```{r, warning=FALSE, message=FALSE}
outdoor <- c('Outdoor', 'Outdoors', 'Cloudy', 'Heinz Field', 
             'Outdor', 'Ourdoor', 'Outside', 'Outddors', 
             'Outdoor Retr Roof-Open', 'Oudoor', 'Bowl')

indoor_closed <- c('Indoors', 'Indoor', 'Indoor, Roof Closed', 'Indoor, Roof Closed',
                   'Retractable Roof', 'Retr. Roof-Closed', 'Retr. Roof - Closed', 'Retr. Roof Closed')

indoor_open <- c('Indoor, Open Roof', 'Open', 'Retr. Roof-Open', 'Retr. Roof - Open')

dome_closed <- c('Dome', 'Domed, closed', 'Closed Dome', 'Domed', 'Dome, closed')

dome_open <- c('Domed, Open', 'Domed, open')

convert_stadiums <- function(x) {
  if(x %in% outdoor) {
    "outdoor"
  } else if(x %in% indoor_closed) {
    "indoor closed"
  } else if(x %in% indoor_open) {
    "indoor open"
  } else if(x %in% dome_closed) {
    "dome_closed"
  } else if( x %in% dome_open) {
    "dome_open"
  } else {
    "unknown"
  }
  
}    

play_list <- play_list %>% 
  mutate(StadiumType = mapply(convert_stadiums, StadiumType))
```

### Weather

Similarly the weather.

In the raw data provided, there are `r length(unique(play_list$Weather))` different weather types categoriesd... 

```{r, warning=FALSE, message=FALSE}
play_list %>% 
  count(Weather) %>% 
  rename(Count = n) %>% 
  mutate(Count = comma(Count)) %>% 
  kableExtra::kable(format = "html", escape = F) %>%
  kableExtra::kable_styling("striped", full_width = F) %>% 
  kableExtra::scroll_box(height = "500px") %>%
  kableExtra::kable_styling(fixed_thead = T)
```


I will also condense these in to five groups, in addition to "Unknown" for those types where the weather variable is missing:
  
  * `rain`
* `overcast`
* `clear`
* `snow`
* `none`


```{r, warning=FALSE, message=FALSE}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ----- condense GameWeather variable: -----#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rain <- c('30% Chance of Rain', 'Rainy', 'Rain Chance 40%', 'Showers', 'Cloudy, 50% change of rain', 'Rain likely, temps in low 40s.',
          'Cloudy with periods of rain, thunder possible. Winds shifting to WNW, 10-20 mph.',
          'Scattered Showers', 'Cloudy, Rain', 'Rain shower', 'Light Rain', 'Rain')

overcast <- c('Party Cloudy', 'Cloudy, chance of rain',
              'Coudy', 
              'Cloudy and cold', 'Cloudy, fog started developing in 2nd quarter',
              'Partly Clouidy', 'Mostly Coudy', 'Cloudy and Cool',
              'cloudy', 'Partly cloudy', 'Overcast', 'Hazy', 'Mostly cloudy', 'Mostly Cloudy',
              'Partly Cloudy', 'Cloudy')

clear <- c('Partly clear', 'Sunny and clear', 'Sun & clouds', 'Clear and Sunny',
           'Sunny and cold', 'Sunny Skies', 'Clear and Cool', 'Clear and sunny',
           'Sunny, highs to upper 80s', 'Mostly Sunny Skies', 'Cold',
           'Clear and warm', 'Sunny and warm', 'Clear and cold', 'Mostly sunny',
           'T: 51; H: 55; W: NW 10 mph', 'Clear Skies', 'Clear skies', 'Partly sunny',
           'Fair', 'Partly Sunny', 'Mostly Sunny', 'Clear', 'Sunny')

snow <- c('Cloudy, light snow accumulating 1-3"', 'Heavy lake effect snow', 'Snow')

none <- c('N/A Indoor', 'Indoors', 'Indoor', 'N/A (Indoors)', 'Controlled Climate')

convert_weather <- function(x) {
  if(x %in% rain) {
    "rain"
  } else if(x %in% overcast) {
    "overcast"
  } else if(x %in% clear) {
    "clear"
  } else if(x %in% snow) {
    "snow"
  } else if( x %in% none) {
    "indoors"
  } else {
    "unknown"
  }
  
}    

play_list <- play_list %>% 
  mutate(Weather = mapply(convert_weather, Weather))

```

***
  
  # Exploratory Data Analysis
  
  In the following section, I will aim to unpack some of the relationships between the happenings of an NFL game and lower-limb injuries.

## Injury Record Data

With almost 86% of the recorded injuries in our dataset, the knees and ankles (46% and 40% respectively) are by far the most frequently injured body part, while the heel is the least frequent, with only one recorded injury.

```{r, message=FALSE, warning=FALSE}
injury_record %>% 
  count(BodyPart) %>% 
  ggplot(aes(x= reorder(BodyPart,n), y= n)) +
  geom_col(fill = plot_cols[1], colour = plot_cols[1], alpha = 0.6) +
  labs(x= "Body Part", y= "Num Injuries") +
  ggtitle("NOT ALL NFL BODY PARTS ARE CREATED EQUAL", subtitle = "86% of the injury records made up by knees and ankles") +
  coord_flip() +
  theme_jason()
```


Of the 105 injuries, 54% (57 occurrences) occurred on Synthetic playing surfaces. This is interesting, and suggests that lower-limb injuries may have some relationship with the playing surface, as 58% of the games in the provided dataset are played on natural playing surfaces. If a greater proportion of games are played on natural surfaces, while a greater proportion of injuries occur on synthetic, what is that telling us?
  
  ```{r, message=FALSE, warning=FALSE}
p1 <- play_list %>% 
  distinct(GameID, FieldType) %>% 
  count(FieldType) %>% 
  ggplot(aes(x= FieldType, y= n)) +
  geom_col(fill = plot_cols[3], colour = plot_cols[3], alpha = 0.6) +
  labs(x= "Surface", y= "Games Played") +
  ggtitle("MORE GAMES PLAYED ON NATURAL TURF", subtitle = "58% of games in our data played on Natural Turf") +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  theme_jason()

p2 <- injury_record %>% 
  count(Surface) %>% 
  ggplot(aes(x= Surface, y= n)) +
  geom_col(fill = plot_cols[1], colour = plot_cols[1], alpha = 0.6) +
  labs(x= "Surface", y= "Num Injuries") +
  ggtitle("SLIGHT DIFFERENCE IN SURFACE OCCURRENCES", subtitle = "Synthetic surfaces have slightly more injuries in absolute terms") +
  coord_flip() +
  theme_jason()


gridExtra::grid.arrange(p1, p2)
```


It's telling us that of the data we've been given to analyse, injuries do occur with greater frequency on synthetic playing surfaces.

Over the whole dataset of over 5,700 games, injuries occurred in 1.8% of those games. When we split that between natural vs synthetic, we can see that games played on synthetic surfaces result in injury 2.33%, while on natural surfaces, injuries only occur in 1.45% of games.

```{r}
# create a variable to indicate whether an injury occurred 
play_list <- play_list %>% 
  mutate(IsInjured = GameID %in% injury_record$GameID)


# play_list %>% 
#   distinct(GameID, FieldType, IsInjured) %>% 
#   count(IsInjured) %>% 
#   mutate(prop = n/sum(n))

play_list %>% 
  distinct(GameID, FieldType, IsInjured) %>% 
  group_by(FieldType, IsInjured) %>% 
  summarise(n = n()) %>% 
  mutate(perc_injured = n / sum(n)) %>% 
  filter(IsInjured == TRUE) %>% 
  ggplot(aes(x= FieldType, y= perc_injured)) +
  geom_col(alpha = 0.6, fill = plot_cols[1], colour = plot_cols[1]) +
  geom_text(aes(label = percent(perc_injured)), vjust=1, size = 7, colour = plot_cols[8]) +
  geom_hline(yintercept = 0.0182, linetype = 2, colour = plot_cols[3]) +
  annotate(geom = "text", x=0.8, y= 0.0192, label = "Injuries occur on\n1.8% of all plays", colour = plot_cols[3], size = 5) +
  ggtitle("MORE LIKELY TO BE INJURED ON SYNTHETIC SURFACES", subtitle = "Of over 5,700 plays analysed, injuries\noccured more frequently on Synthetic") +
  theme_jason() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())

```


Of the 42 ankle injuries, 60% occurred on synthetic turf, while the 48 recorded knee injuries occurred with identical frequency on both playing surfaces.

If we look further down the list, feet and toes are direct opposites; 71% of foot injuries occurred on natural playing surfaces, while 86% of toe injuries occurred on Synthetic surfaces. Care should be taken with these two lower-limb parts, as both only have seven recorded injuries events each.

```{r, message=FALSE, warning=FALSE}
injury_record %>% 
  ggplot(aes(x=BodyPart, fill = Surface, colour = Surface)) +
  geom_bar(stat = "count", position = "fill", alpha = 0.6) +
  scale_fill_manual(values = plot_cols[c(2,3)]) +
  scale_colour_manual(values = plot_cols[c(2,3)]) +
  scale_y_continuous(labels = percent) +
  ggtitle("SURFACE AND BODY TYPE SHOW SOME RELATIONSHIP", subtitle = "60% of ankle injuries occur on Synthetic, while\nknee injuries occur with identical frequency on both surfaces") +
  theme_jason() +
  theme(axis.title.y = element_blank())
```




```{r, message=FALSE, warning=FALSE}
# create a variable to indicate the severity of the injury (time missed)
injury_record <- injury_record %>% 
  mutate(severity = ifelse(DM_M42 == 1, "42", 
                           ifelse(DM_M42 == 0 & DM_M28 == 1, "28",
                                  ifelse(DM_M42 == 0 & DM_M28 == 0 & DM_M7 == 1, "7", "1"))))
```


Of the seven recorded foot injuries, no player was able to return to the field in less than 28 days.

When focusing on the ankle and knees, the rate at which players returned within the first 28 days was almost identical, with the only difference coming in ankle injuries having a slightly higher occurrence of returning within the first 7 days than knees.


```{r, message=FALSE, warning=FALSE}
injury_record %>% 
  # count(Surface, DM_M42) %>% 
  ggplot(aes(x=BodyPart, fill = factor(severity, levels = c("42", "28", "7", "1")), colour = factor(severity, levels = c("42", "28", "7", "1")))) +
  geom_bar(stat = "count", position = "fill", alpha = 0.6) +
  scale_fill_manual(values = plot_cols, name = "Severity") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  scale_y_continuous(labels = percent) +
  ggtitle("SOME DIFFERENCES BETWEEN BODY PART AND SEVERITY", subtitle = "Foot injuries generally longer recovery times") +
  theme_jason() +
  theme(axis.title.y = element_blank())
```


We can see that the time missed due to injury is fairly consistent between the two playing surfaces, so while injuries appear to occur more frequently on synthetic, there doesn't appear to be a relationship between these two variables.

```{r, message=FALSE, warning=FALSE}
injury_record %>% 
  # count(Surface, DM_M42) %>% 
  ggplot(aes(x=Surface, fill = factor(severity, levels = c("42", "28", "7", "1")), colour = factor(severity, levels = c("42", "28", "7", "1")))) +
  geom_bar(stat = "count", position = "fill", alpha = 0.6) +
  scale_fill_manual(values = plot_cols, name = "Severity") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  scale_y_continuous(labels = percent) +
  ggtitle("MINIMAL DIFFERENCES IN SEVERITY BASED ON SURFACE", subtitle = "The time missed because of injury doesn't really differ\nbetween playing surfaces") +
  theme_jason() +
  theme(axis.title.y = element_blank())
```


## Injury Record and Play List Data

```{r, message=FALSE, warning=FALSE}
# because there are some injuries without a playID, I will derive the player's position from the play_list df,
# then join on to injury_record
a <- play_list %>% filter(!is.na(Position), Position != "Missing Data") %>% distinct(PlayerKey, Position)

# join to data
injury_record <- injury_record %>% left_join(a, by = "PlayerKey")

# remove the duplicate occurrence of where players have multiple positions
injury_record <- injury_record %>% 
  # mutate(Position = ifelse(is.na(Position), CleanPosition, Position)) %>% 
  distinct(PlayKey, GameID, BodyPart, .keep_all = T) 


# join variables that can be joined at the game level:
injury_record <- injury_record %>% 
  left_join(play_list %>% select(GameID, StadiumType, FieldType, Temperature, Weather), by = "GameID") %>% 
  distinct(.keep_all = T)


# join per play variables
injury_record <- injury_record %>% 
  left_join(play_list %>% select(PlayKey, PlayerDay, PlayerGame, PlayType, PlayerGamePlay), by = "PlayKey")

# fill in missing game number variable where there was no playID in the injury report
injury_record <- injury_record %>% 
  mutate(PlayerGame = ifelse(is.na(PlayerGame), as.numeric(str_extract(GameID, "[^-]*$")), PlayerGame))
```

Wide Recievers, Outside Linebackers and Cornerbacks have the highest number of lower limb injuries.

When analysing the lower-limb body type injured by position, we can see that Outside and Inside Linebackers injure their kness with greater frequencyes, while Middle Linebackers injur their ankles with greater frequency.

```{r, message=FALSE, warning=FALSE, fig.height=9}
p1 <- injury_record %>% 
  count(Position) %>% 
  ggplot(aes(x= reorder(Position,n), y= n)) +
  geom_col(fill = plot_cols[1], colour = plot_cols[1], alpha = 0.6) +
  ggtitle("SOME PLAYERS INJURED MORE OFTEN THAN OTHERS", subtitle = "Wide Receivers and Outside Linebackers\nthe most frequently injured") +
  labs(x= "Position on Play", y= "Injury Count") +
  coord_flip() +
  theme_jason()
  
p2 <- injury_record %>% 
  count(Position, BodyPart) %>% 
  ggplot(aes(x= reorder(Position,n), y= n, fill = BodyPart, colour = BodyPart)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.6) +
  scale_fill_manual(values = plot_cols, name = "Body Part") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  scale_y_continuous(labels = percent) +
  ggtitle("OLBs AND ILBs INJURE KNEES, MLBs INJURE ANKLES?", subtitle = "Body part injured seems to differ\nbased on player's position on the play") +
  labs(x= "Position on Play", y= "Injury Proportion") +
  coord_flip() +
  theme_jason()

gridExtra::grid.arrange(p1, p2)
```



While there were more overall punt plays on natural surfaces (`r comma(sum(play_list$PlayType[play_list$FieldType == "Natural"] == "Punt"))` to `r comma(sum(play_list$PlayType[play_list$FieldType == "Synthetic"] == "Punt"))`), the proportion of those plays that ended with injury were greater on synthetic surfaces (3% to 2%). The same holds true for Extra Point plays. This might be something we need to consider.

```{r, message=FALSE, warning=FALSE}
play_list %>% 
  filter(!PlayType %in% c("", "0")) %>% 
  group_by(PlayType, FieldType, IsInjured) %>% 
  summarise(n_Plays = n_distinct(PlayKey)) %>% 
  mutate(proportion_plays = round(n_Plays / sum(n_Plays), 2)) %>% group_by(PlayType, FieldType) %>% mutate(total_plays = sum(n_Plays)) %>% ungroup() %>%
  filter(IsInjured == TRUE) %>%
  ggplot(aes(x= reorder(PlayType, total_plays), y= total_plays)) +
  geom_col(fill = plot_cols[3], colour = plot_cols[3], alpha = 0.6) +
  geom_text(aes(label = percent(proportion_plays)), hjust=-0, size =5, colour = plot_cols[8]) +
  scale_y_continuous(labels = comma, name = "Total Plays") +
  ggtitle("SPECIAL TEAMS PLAYS ON SYNTHETIC MORE SUSCEPTIBLE TO INJURY", subtitle = "Greater proportion of plays involving punts and PATs\nend in injury on Synthetic") +
  coord_flip() +
  theme_jason() +
  theme(axis.title.y = element_blank()) +
  facet_wrap(~ FieldType)
```



The median play number that players get injured on synthetic playing surfaces is 22, while this number is lower on natural surfaces, with injuries occurring on the 16th play.


```{r, message=FALSE, warning=FALSE}
injury_record %>% 
  ggplot(aes(x= FieldType, y= PlayerGamePlay, fill = FieldType, colour = FieldType)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = plot_cols, guide = "none") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  ggtitle("INJURIES OCCUR EARLIER ON NATURAL SURFACES", subtitle = "28 records not included as exact play\nwhere injury occurred is unknown") +
  labs(x= "Field Type", y= "Game Play") +
  theme_jason()
```



We can see that the number of injuries decrease as we progress through the games in the two year period analysed. This would be interesting, however when plotting the number of players in the `Play List` data set, we can see that they are also steadily decreasing through the games.

```{r, warning=FALSE, message=FALSE}
p1 <- injury_record %>% 
  group_by(PlayerGame) %>% summarise(num_players = n_distinct(PlayerKey)) %>% 
  ggplot(aes(x=PlayerGame, y=num_players)) +
  geom_line(colour = plot_cols[1], size = 1) +
  geom_point(colour = plot_cols[1], size = 3) +
  labs(x= "Game Number", y= "Number Injuries") +
  ggtitle("NUMBER OF INJURIES DECREASING THROUGH GAMES", subtitle = "Week 3 has the most injuries (n=9)") +
  theme_jason()


p2 <- play_list %>% 
  group_by(PlayerGame) %>% summarise(num_players = n_distinct(PlayerKey)) %>% 
  ggplot(aes(x=PlayerGame, y=num_players)) +
  geom_line(colour = plot_cols[3], size = 1) +
  geom_point(colour = plot_cols[3], size = 3) +
  labs(x= "Game Number", y= "Number Players") +
  ggtitle("NUMBER OF PLAYERS ALSO DECREASING", subtitle = "Need to see whether the proportion is increasing or decreasing") +
  theme_jason()

gridExtra::grid.arrange(p1, p2, ncol = 1)
```

To further drill down into the issue of whether there is a true difference in the game number we can calculate the proportion of injuries (`num_injured` / `num_players`).

When we do this, we can see that based on the data provided, it does appear that players may be more likely to get injured over the earlier games, peaking in week 3 at 3.6%.

```{r, warning=FALSE, message=FALSE}
injury_record %>% 
  group_by(PlayerGame) %>% summarise(num_injured = n_distinct(PlayerKey)) %>% ungroup() %>% 
  left_join(play_list %>% group_by(PlayerGame) %>% summarise(num_players = n_distinct(PlayerKey)) %>% ungroup(), by = "PlayerGame") %>% 
  mutate(prop_injured = num_injured / num_players) %>% 
  ggplot(aes(x=PlayerGame, y=prop_injured)) +
  geom_line(colour = plot_cols[2], size = 1) +
  geom_point(colour = plot_cols[2], size = 3) +
  labs(x= "Game Number", y= "Injury Rate") +
  scale_y_continuous(labels = percent) +
  ggtitle("PLAYERS MORE LIKELY INJURED BY THE 8th GAME", subtitle = "The injury rate peaks at week 3 to 3.6%") +
  theme_jason()
```

There doesn't appear to be much of a relationship between the temperature and whether a player gets injured. The median temperature is ever so slightly higher when the player is injured (62.5 to 61)

```{r}
play_list %>% 
  filter(StadiumType %in% c("dome_open", "indoor open", "outdoor")) %>% 
  filter(Temperature != -999) %>% 
  distinct(GameID, Temperature, IsInjured) %>% 
  ggplot(aes(y= Temperature, x = IsInjured, fill = IsInjured, colour = IsInjured)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = plot_cols, guide = "none") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  labs(x= "Player Injured?") +
  ggtitle("TEMPERATURE ISN'T A BIG FACTOR", subtitle = "While injuries tend to occur in slightly higher\ntemperatures, doesn't look significant") +
  coord_flip() +
  theme_jason()
```


Games played in the rain result in more injuries than any other weather type, with 2% of those plays ending in injury. 

Also of note, of the data provided, no injuries occurred in the snow. 

```{r, warning=FALSE, message=FALSE}
play_list %>% 
  filter(Weather != "indoors") %>% 
  group_by(Weather, IsInjured) %>% 
  summarise(n_Plays = n_distinct(PlayKey)) %>% 
  mutate(proportion_plays = round(n_Plays / sum(n_Plays), 2)) %>% 
  filter(IsInjured == TRUE) %>%
  ggplot(aes(x= reorder(Weather, n_Plays), y= proportion_plays, fill = IsInjured, colour = IsInjured)) +
  geom_col(alpha = 0.6) +
  scale_fill_manual(values = plot_cols, guide = "none") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  geom_text(aes(label = percent(proportion_plays)), hjust= 1, size =5, colour = plot_cols[8]) +
  scale_y_continuous(labels = comma, name = "% Plays") +
  ggtitle("INJURIES MORE LIKELY TO OCCUR IN THE RAIN", subtitle = "No injuries in the snow is a surprise though") +
  labs(caption = "*Indoor stadiums excluded") +
  coord_flip() +
  theme_jason() +
  theme(axis.title.y = element_blank(), axis.text.x = element_blank(), plot.caption = element_text(colour = "darkgrey"))
```


Games played in indoor stadiums with the retractable roof open appear to result in more injuries than other stadium types, with injuries on 3% of all plays.

```{r, warning=FALSE, message=FALSE}
play_list %>% 
  group_by(StadiumType, IsInjured) %>% 
  summarise(n_Plays = n_distinct(PlayKey)) %>% 
  mutate(proportion_plays = round(n_Plays / sum(n_Plays), 2)) %>% 
  filter(IsInjured == TRUE) %>%
  ggplot(aes(x= reorder(StadiumType, n_Plays), y= proportion_plays, fill = IsInjured, colour = IsInjured)) +
  geom_col(alpha = 0.6) +
  scale_fill_manual(values = plot_cols, guide = "none") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  geom_text(aes(label = percent(proportion_plays)), hjust= 1, size =5, colour = plot_cols[8]) +
  scale_y_continuous(labels = comma, name = "% Plays") +
  ggtitle("WHY ARE WE OPENING THE ROOF?!", subtitle = "Injuries are more likely to occur in indoor stadiums with the roof open") +
  coord_flip() +
  theme_jason() +
  theme(axis.title.y = element_blank(), axis.text.x = element_blank())
```


When we break that out into the different field types, we can see that games played on synthetic turf in indoor stadiums are by far the most problematic, with 7% of the plays on this surface resulting in an injury.

```{r}
play_list %>% 
  group_by(StadiumType, FieldType, IsInjured) %>% 
  summarise(n_Plays = n_distinct(PlayKey)) %>% 
  mutate(proportion_plays = round(n_Plays / sum(n_Plays), 2)) %>% 
  filter(IsInjured == TRUE) %>%
  ggplot(aes(x= reorder(StadiumType, n_Plays), y= proportion_plays, fill = FieldType, colour = FieldType)) +
  geom_col(alpha = 0.6) +
  scale_fill_manual(values = plot_cols, guide = "none") +
  scale_colour_manual(values = plot_cols, guide = "none") +
  geom_text(aes(label = percent(proportion_plays)), hjust= 1, size =5, colour = plot_cols[8]) +
  scale_y_continuous(labels = comma, name = "% Plays") +
  ggtitle("INDOOR OPEN STADIUMS ON SYNTHETIC ARE BAD NEWS", subtitle = "Injuries are far more likely here, with 7% injury rate") +
  coord_flip() +
  theme_jason() +
  theme(axis.title.y = element_blank(), axis.text.x = element_blank()) + 
  facet_wrap(~ FieldType)
```

***
  
  ## Player Tracking Data
  
  ### Pre-Processing
  
  Player tracking data underwent pre-processing to get it in to a useable state.

Some of the steps taken are:
  
  * Variable created indicating whether an injury occured on the play:
  
  ```{r, warning=FALSE, message=FALSE, include=TRUE}
player_tracking <- player_tracking %>% 
  mutate(isInjured = PlayKey %in% injury_record$PlayKey) 
```

* Missing values in the `event` variable were filled in:
  
  ```{r, warning=FALSE, message=FALSE}
player_tracking %>% head(10) %>% 
  kableExtra::kable(format = "html", escape = F) %>%
  kableExtra::kable_styling("striped", full_width = F) %>% 
  kableExtra::scroll_box(height = "500px", width = "900px") %>%
  kableExtra::kable_styling(fixed_thead = T) 
```


```{r, warning=FALSE, message=FALSE, echo=TRUE}
# solution for the below function taken from here: https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
repeat.before <- function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}  

player_tracking$event <- ifelse(player_tracking$event == "", NA, player_tracking$event)
player_tracking <- player_tracking %>% group_by(PlayKey) %>% mutate(event_clean = repeat.before(event)) %>% ungroup()
```


### In-Play vs Dead Ball

* Variable (`play_stage`) created to indicate whether the `event` was a play event, or whether it was either pre (*huddle*, *line_set*, etc) or post play (*timeout*, *timeout_booth_review*, etc).

The following values were used to classify an event as *in_play*:
  
  >"tackle", "ball_snap", "pass_outcome_incomplete", "out_of_bounds", "first_contact", "handoff", "pass_forward", "pass_outcome_caught", "touchdown", "qb_sack",\n"touchback", "kickoff", "punt", "pass_outcome_touchdown", "pass_arrived", "extra_point", "field_goal", "play_action", "kick_received",\n"fair_catch", "punt_downed", "run", "punt_received", "qb_kneel", "pass_outcome_interception", "field_goal_missed", "fumble", "fumble_defense_recovered", "qb_spike",\n "extra_point_missed", "fumble_offense_recovered", "pass_tipped", "lateral", "qb_strip_sack", "safety", "kickoff_land", "snap_direct", "kick_recovered",\n "field_goal_blocked", "punt_muffed", "pass_shovel", "extra_point_blocked", "pass_lateral", "punt_blocked", "run_pass_option", "free_kick", "punt_fake",\n"end_path", "drop_kick", "field_goal_fake", "extra_point_fake", "xp_fake"



```{r, warning=FALSE, message=FALSE}
# create vector of values for in_play
in_play <- c("tackle", "ball_snap", "pass_outcome_incomplete", "out_of_bounds", "first_contact", "handoff", "pass_forward", "pass_outcome_caught", "touchdown", "qb_sack", "touchback", "kickoff", "punt", "pass_outcome_touchdown", "pass_arrived", "extra_point", "field_goal", "play_action", "kick_received", "fair_catch", "punt_downed", "run", "punt_received", "qb_kneel", "pass_outcome_interception", "field_goal_missed", "fumble", "fumble_defense_recovered", "qb_spike", "extra_point_missed", "fumble_offense_recovered", "pass_tipped", "lateral", "qb_strip_sack", "safety", "kickoff_land", "snap_direct", "kick_recovered", "field_goal_blocked", "punt_muffed", "pass_shovel", "extra_point_blocked", "pass_lateral", "punt_blocked", "run_pass_option", "free_kick", "punt_fake", "end_path", "drop_kick", "field_goal_fake", "extra_point_fake", "xp_fake")

# create variable to indicate whenther the even is in_play or dead_ball
player_tracking <- player_tracking %>% 
  mutate(play_stage = ifelse(event_clean %in% in_play, "in_play", "dead_ball"))
```


### Analysing Player Tracking Data

```{r, warning=FALSE, message=FALSE}
# create a summary dataframe of player tracking data for analysis
player_tracking_summary <- player_tracking %>% 
  filter(play_stage == "in_play") %>% 
  group_by(PlayKey) %>% 
  summarise(min_time = min(time, na.rm = T),
            max_time = max(time, na.rm = T),
            avg_speed = mean(s, na.rm = T),
            sd_speed = sd(s, na.rm = T)) %>% 
  mutate(play_time = max_time - min_time) %>% ungroup()

# join play list data to get additional metadata
player_tracking_summary <- player_tracking_summary %>% 
  left_join(play_list, by = "PlayKey")
```


Once we've isolated the actual in-play tracking data, we can calculate summary statistics for the plays themselves.

The average player speed per play on all plays in the data is 2.17 yards per second.

```{r, warning=FALSE, message=FALSE}
player_tracking_summary %>% 
  ggplot(aes(x= avg_speed)) + 
  geom_density(alpha = 0.5, fill = plot_cols[1], colour = plot_cols[1]) +
  geom_vline(xintercept = mean(player_tracking_summary$avg_speed), linetype = 2, colour = plot_cols[8]) +
  annotate("text", y= 0.4, x= mean(player_tracking_summary$avg_speed) + 1.4, label = paste0("Average Speed = ", round(mean(player_tracking_summary$avg_speed), 2)), size=5, colour = plot_cols[8]) +
  labs(x= "Average Speed (YPS)", y= "Density") +
  ggtitle("AVERAGE SPEED ON PLAYS SLIGHTLY SKEWED", subtitle = "Average speed of players on each play is 2.17 yeards per second") +
  theme_jason()
```


We can see that players appear to be travelling at faster average speeds on plays where an injury occurs (average speed of 2.35 yards per second on natural vs 2.14 and average speed of 2.40 yards per second vs 2.21 on synthetic).

In both injury and non-injury plays, players are travelling at faster speeds on synthetic surfaces.

```{r, warning=FALSE, message=FALSE}
player_tracking_summary %>% 
  ggplot(aes(x= IsInjured, y= avg_speed, fill = FieldType, colour = FieldType)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = plot_cols) +
  scale_colour_manual(values = plot_cols, guide = "none") +
  labs(x= "Player Injured?", y= "Average Speed") +
  ggtitle("SPEED KILLS??", subtitle = "Average Player Speed is higher on plays where injuries occur") +
  coord_flip() +
  theme_jason()
```


This is also the case for the variability (standard deviation) of player speed - injuries occur on plays where there is higher variation in the player's speed.
```{r, warning=FALSE, message=FALSE}
player_tracking_summary %>% 
  ggplot(aes(x= IsInjured, y= sd_speed, fill = FieldType, colour = FieldType)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = plot_cols) +
  scale_colour_manual(values = plot_cols, guide = "none") +
  labs(x= "Player Injured?", y= "Std_Dev Speed") +
  ggtitle("VARIABLE SPEED ALSO KILLS??", subtitle = "Standard deviation of player speed is greater on plays where injuries occur") +
  coord_flip() +
  theme_jason()
```
