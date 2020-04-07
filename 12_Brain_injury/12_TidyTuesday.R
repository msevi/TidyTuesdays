#Load packages

library(tidyverse)
library(skimr)
library(ggwaffle) #https://github.com/liamgilbey/ggwaffle
library(hrbrthemes)


#Get data
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')


#Lollipop plot #https://www.statology.org/how-to-create-a-lollipop-chart-in-r/
#https://towardsdatascience.com/three-key-charts-for-visualizing-proportion-data-4cf58c23d513

ggplot(tbi_military %>% 
         filter(component=="Active") %>%
         filter(severity=="Mild"), aes(x=service, y=diagnosed)) + 
  geom_point(size=4) + 
  geom_segment(aes(x=service, xend=service,  y=0, yend=diagnosed)) + 
  facet_grid(~year) +
  geom_text(color="red", size=4, vjust=-0.8, aes(label=diagnosed)) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank()) +
  ggtitle("Brain injuries of mild severity per year by active memembers of different military groups")


ggplot(tbi_military %>% 
         filter(component=="Active") %>%
         filter(severity=="Mild") %>%
         filter(year=="2010"), aes(x=service, y=diagnosed)) + 
  geom_point(size=4) + 
  geom_segment(aes(x=service, xend=service,  y=0, yend=diagnosed)) + 
  facet_grid(~year) +
  geom_text(color="red", size=4, vjust=-0.8, aes(label=diagnosed)) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank()) +
  ggtitle("Brain injuries of mild severity by active memembers of different military groups for 2010")

#Waffle chart
waffle_data <- waffle_iron(tbi_military %>% 
                             filter(component=="Active") %>%
                             filter(severity=="Mild") %>%
                             filter(year=="2010"), aes_d(group = service))

ggplot(waffle_data , aes(x, y, fill = group)) + 
  geom_waffle() +
  coord_equal() + 
  scale_fill_waffle() + 
  theme_waffle()

#Pie chart [https://rpubs.com/ageek/ggplot-adv-part2]

ggplot(tbi_military %>% 
         filter(component=="Active") %>%
         filter(severity=="Mild") %>%
         filter(year=="2010"), aes(x = "", y=diagnosed, fill = factor(service))) + 
  geom_bar(width = 1, stat="identity") +
  geom_text(aes(label=diagnosed), 
            size=6,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")
  