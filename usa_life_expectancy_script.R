#---------------------------------------------------------------------------#
# Nom : usa_life_expectancy_script.R                          			        #
# Description : Compare life expectancy by state                            #
# Auteur: Pietro Violo                                                      #
# Date : 27 mai 2022                                                        #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

rm(list=ls(all=TRUE))

#'* Libraries *

#devtools::install_github("UrbanInstitute/urbnmapr")

library(tidyverse)
library(viridis)
library(urbnmapr)
library(gganimate)
library(gifski)

#'* Data import and data wrangling *


# States map dataframe

statemap <- urbnmapr::states

# States abbreviations

stateabb <- statemap %>% pull(state_abbv) %>% unique()


# Load life tables
statedata <- c()

for(state in stateabb){
  statedata <- rbind(statedata,read.csv(paste("./lifetables/States/",state,"/",
                                              state,"_bltper_1x1.csv", sep ="")))
}

statedata <- statedata %>% tibble(.)

# Keep only life expectancy at birth

statedata <- statedata %>% filter(Age == 0) %>% 
  rename(state_abbv = PopName)

# Left join with map data

statemap_df <- full_join(statedata, statemap, by = "state_abbv")
statemap_df <- statemap_df %>% mutate(year = Year)

#'* Plot graph *

#png("usa_life_expectancy_2019.png", res = 300, width = 4400, height = 2600)

ggmap <- ggplot(data = statemap_df,
       aes(x = long, y = lat, group = group, fill = ex)) +
  geom_polygon() +
  ggtitle("Life expectancy at birth in the United States, 1959-2019",
          subtitle = "Life expectancy is defined as the average number of years that a newborn could expect to live,
          \n if he or she were to pass through life exposed to the sex- and age-specific death rates prevailing at the time of his or her birth,
          \n for a specific year, in a given country, territory, or geographic area (World Health Organization definition).")+
  coord_map(projection = "albers", lat = 45, lat1 = 55) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))) +
  scale_fill_viridis(name = "Life expectancy at birth")
    

graph.animation <- ggmap +
  transition_states(year, state_length = 10)+
  ease_aes("back-out") +
  geom_text(aes(label=paste("Year : ",year)), color = "darkslategrey") 

#dev.off()



animate(graph.animation, height = 1000, width = 1800, fps = 40, duration = 20, res = 200)

anim_save("PopulationPyramid.gif")

