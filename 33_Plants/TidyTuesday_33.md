Tidy\_Tuesday\_2020\_08\_25
================

Tidy Tuesday: Plants in danger
==============================

Load libraries
--------------

    library(tidyverse)
    library(ggtext)
    library(hrbrthemes)
    library(paletteer)
    library(skimr)

Get Data
--------

    plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

Data Wrangling
--------------

    counts_plants<- plants %>%
      drop_na() %>%
      group_by(continent, group, year_last_seen) %>%
      summarise(counts=n()) %>%
      ungroup() %>%
      mutate(year_last_seen = fct_relevel(year_last_seen, c("Before 1900" , "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999","2000-2020")))

      
    total_plants<- counts_plants %>%
      group_by(continent, year_last_seen) %>%
      summarise(total=sum(counts)) %>%
      ungroup() %>%
      mutate(year_last_seen = fct_relevel(year_last_seen, c("Before 1900" , "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999","2000-2020")))

    counts_plants <- counts_plants %>%
      left_join(total_plants) %>%
      mutate(proportion= counts/total)

    counts_plants2 <- plants %>%
      drop_na() %>%
      arrange(group) %>%
      group_by(continent, year_last_seen) %>%
      mutate(index = row_number()) %>%
      ungroup() %>%
      mutate(year_last_seen = fct_relevel(year_last_seen, c("Before 1900" , "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999","2000-2020")))

Visualizations
--------------

    ggplot(counts_plants) +
      geom_bar(aes(year_last_seen, proportion, fill=group), stat = "identity") +
      facet_grid(continent~.) +
      theme_void() +
      labs(title = "Proportion of plants last seen per decade and continent") +
      theme(axis.text.x = element_text(face = "bold", size = 10)) +
      theme(plot.title = element_markdown(face="bold")) +
      theme(strip.text=element_text(face = "bold", hjust=0)) +
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
      theme(legend.position = "bottom", legend.margin = margin(), legend.title = element_blank()) +
      guides(fill = guide_legend(nrow = 1)) +
      scale_fill_paletteer_d("vapoRwave::floralShoppe", guide="none")

![](TidyTuesday_33_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

    ggplot() +
      geom_point(data= counts_plants2, aes(x=continent, y=index, shape=group, color=continent), stat = "identity") +
      facet_grid(~year_last_seen, switch= "x") +
      theme_void() +
      labs(title = "Plants last seen by decade in **Africa**, <span style='color:#77ACDE;'>**Asia**</span>, <span style='color:#E1B9D8;'>**Europe**</span>,  <span style='color:#CE7D84;'>**North America**</span>,  <span style='color:#FCA270;'>**Oceania**</span>, and  <span style='color:#FFE2D0;'>**South America**</span>",
           subtitle = "Shapes denote plant grouping and the total number of plants is provided for each continent") +
      theme(plot.title = element_markdown(face="bold")) +
      theme(strip.text=element_text(face = "bold", hjust=0.5)) +
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
      theme(legend.position = "bottom",  legend.margin = margin(), legend.title = element_blank()) +
      guides(shape = guide_legend(nrow = 1)) +
      scale_color_paletteer_d("vapoRwave::floralShoppe", guide="none") +
      geom_text(data=total_plants, aes(continent, total + 2, label=total))

![](TidyTuesday_33_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->