## -- Tidy Tuesday, Week 2 of 2024 -- ##

## -- load libraries -- ##
library(lubridate) # for dates
library(tidyverse) # for data wrangling + plotting
library(tidytuesdayR) # for the data


## -- load full data -- ##
tuesdata <- tidytuesdayR::tt_load(2024, week = 2)


## -- load split data -- ##
canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births
nhl_rosters <- tuesdata$nhl_rosters
nhl_teams <- tuesdata$nhl_teams


## -- data wrangling -- ## 

# create function for calculating zodiac signs

#Note: 
# the following function is a slightly modified version of the source code from the 
# "DescTools" pckg and its Zodiac function. I modified it because I didn't want to install the whole pckg
  # Pckg url: https://andrisignorell.github.io/DescTools/index.html
  # Original code: https://github.com/AndriSignorell/DescTools/blob/master/R/DescTools.r#L3527-L3547
  # Citation: Signorell A (2023). DescTools: Tools for Descriptive Statistics. R package version 0.99.51, https://github.com/AndriSignorell/DescTools/, https://andrisignorell.github.io/DescTools/.

zodiac <- function(x, stringsAsFactors = TRUE) {
  
  z <- c("Capricorn","Aquarius","Pisces","Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn")
  
  i <- cut(lubridate::month(x) * 100 + lubridate::day(x), 
           breaks = c(0,120,218,320,420,520,621,722,823,922,1023,1122,1222,1231), 
           right=FALSE, include.lowest = TRUE)
  
  if(stringsAsFactors){
    res <- i
    levels(res) <- z
  } else {
    res <- z[i]
  }
  return(res)
}

# create table that also has the elements and qualities of the zodiacs
astro_extras <-  data.frame("sign" = c("Aries", "Taurus", "Gemini", "Cancer",
                                          "Leo", "Virgo", "Libra", "Scorpio", 
                                          "Sagittarius", "Capricorn", "Aquarius", "Pisces"),
                                         
                            "element" = rep(c("Fire", "Earth", "Air", "Water"), 3),
                                         
                            "qualities" = rep(c("Cardinal", "Fixed", "Mutable"), 4))


## -- create Avalanche Roster data frame -- ##
# subset avalanche from overall NHL roster
# calculate zodiac sign based on birth date
# add an element and quality column according to zodiac
avalanche <- nhl_rosters %>% 
  filter(team_code == "COL") %>% 
  mutate(zodiac_sign = zodiac(birth_date)) %>% 
  mutate(zodiac_element = NA) %>% 
  mutate(zodiac_quality = NA)


# fill in the element column in avalanche data frame
for(i in 1:length(avalanche$zodiac_sign)){
  for(j in 1:length(astro_extras$sign)){
    if(avalanche$zodiac_sign[i] == astro_extras$sign[j]){
      avalanche$zodiac_element[i] = astro_extras$element[j]
    }
  }
}


# fill in quality column in avalanche data frame
for(i in 1:length(avalanche$zodiac_sign)){
  for(j in 1:length(astro_extras$sign)){
    if(avalanche$zodiac_sign[i] == astro_extras$sign[j]){
      avalanche$zodiac_quality[i] = astro_extras$qualities[j]
    }
  }
}


## -- create summary tables -- ##
# get a summary table of what their team positions looks like
avalanche_position_count <- avalanche %>% 
  group_by(position_type) %>% 
  summarise(count = n())

avalanche_zodiac_count <- avalanche %>% 
  group_by(zodiac_sign) %>% 
  summarise(count = n())

position_sign_summary <- avalanche %>% 
  group_by(position_type, zodiac_sign) %>% 
  summarise(count = n())

position_element_summary <- avalanche %>% 
  group_by(position_type, zodiac_element) %>% 
  summarise(count = n())

position_quality_summary <- avalanche %>% 
  group_by(position_type, zodiac_quality) %>% 
  summarise(count = n())


## -- create visualizations -- ##
# visualize the position_element summary table, facet wrap by position
ggplot(avalanche, aes(x = zodiac_element, fill = zodiac_element)) +
  geom_bar() +
  scale_fill_manual(values = c("Air" = "gold2", "Earth" = "olivedrab",
                              "Fire" = "tomato3", "Water" = "steelblue4")) +
  facet_wrap(~position_type) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 200)) +
  labs(
    title = "Which Zodiac Elements Occur Most Frequently in Hockey Positions 
on the Colorado Avalanche?",
    subtitle = "The data below shows the cumulative number of players on the Colorado Avalanche’s roster through time. 
They were divvied up according to their zodiac sign’s element, and their position on the team. Air signs
include Gemini, Libra, and Aquarius. Earth signs include Taurus, Virgo, and Capricorns. Fire signs include
Aries, Leo, and Sagittarius. And finally, Water signs include Cancer, Scorpio, and Pisces. \n",
    y = "No. of Players\n",
    x = "\nZodiac Elements",
    caption = "Tidy Tuesday: Week 2, 2024
    Daphne Virlar-Knight | Data: NHL API"
  ) +
  theme(legend.position = "none",
      text = element_text(family = "Roboto Condensed"), # had to download this font for it to work
      plot.title = element_text(family = "Noto Sans",
                                face = "bold",
                                size = 14),
      plot.background = element_rect(fill = "grey99"))
