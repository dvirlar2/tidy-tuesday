## -- Tidy Tuesday, Week 2 of 2024 -- ##

## -- load libraries -- ##
library(lubridate) # for dates
library(tidyverse) # for data wrangling + plotting
library(tidytuesdayR) # for the data


## -- load data -- ##
tuesdata <- tidytuesdayR::tt_load(2024, week = 2)


## -- clean data -- ##
canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births
nhl_rosters <- tuesdata$nhl_rosters
nhl_teams <- tuesdata$nhl_teams


## -- explore data -- ##
# canada_births_1991_1999 <- canada_births_1991_2022 %>%
#   filter(year <= 1999)
# 
# ggplot(canada_births_1991_1999, aes(x = month, y = births)) +
#   geom_col() +
#   facet_wrap(~ year)


## -- idea generation -- ##
# graph births per month over time for colorado rockies (CLR), avalanche (COL), or 
# the Seattle Krakens (SEA). 

# could also summarize number of births into seasons

# it might be interesting to see what the total number of births per season 
# across time (so sum all Januaries together, all Febs, etc.), and see how many
# of what birth month tend to go to what positions

# OH! EVEN BETTER! Break it up into astrological signs! Do certain signs tend to be more 
# aggressive or defensive in nature?? 
  # maybe use the team's color palette (get color palette from jerseys/logos) for plotting


## -- data wrangling -- ## 

# create function for calculating zodiac signs

#Note: 
# the following code is a slightly modified version of the source code from the 
# "DescTools" pckg and it's Zodiac function. I modified it because I didn't want to install the whole pckg
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

{
# create data frame for astrological signs
astro_signs <- data.frame("Sign" = c("Aries", "Taurus", "Gemini", "Cancer",
                                     "Leo", "Virgo", "Libra", "Scorpio", 
                                     "Sagittarius", "Capricorn", "Aquarius", "Pisces"),
                          
                          "Element" = rep(c("Fire", "Earth", "Air", "Water"), 3),
                          
                          "Qualities" = rep(c("Cardinal", "Fixed", "Mutable"), 4),
                          # 
                          # "Beginning_Date" = c("March 21", "April 20", "May 21", "June 22",
                          #                      "July 23", "August 23", "September 23", "October 24",
                          #                      "November 22", "December 22", "January 20", "February 19"),
                          # 
                          # "End_Date" = c("April 19", "May 20", "June 21", "July 22",
                          #                "August 22", "September 22", "October 23", "November 21",
                          #                "December 21", "January 19", "February 18", "March 20")
                          )


# zodiac_signs <- astro_signs %>% 
#   mutate(Beginning_Date = lubridate::parse_date_time(astro_signs$Beginning_Date, "md")) %>% 
#   mutate(End_Date = lubridate::parse_date_time(astro_signs$End_Date, "md"))
# 
# 
# zodiac_dates <- zodiac_signs %>% 
#   mutate(zb_year = as_date(1900)) %>% 
#   mutate(zb_month = month(Beginning_Date)) %>% 
#   mutate(zb_day = day(Beginning_Date)) %>% 
#   mutate(ze_month = month(End_Date)) %>% 
#   mutate(ze_day = day(End_Date))
#   
}


## -- create Avalanche Roster data frmae -- ##

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
