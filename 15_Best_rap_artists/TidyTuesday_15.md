Tidy\_Tuesday\_2020\_04\_21
================

Tidy Tuesday: Best Rap Artists
==============================

This week's objective is to visualize [data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-14/readme.md) on hip hop songs rankings.

I came across [this](https://resources.rstudio.com/rstudio-conf-2020/the-glamour-of-graphics-william-chase?wvideo=fhb2ifduim) presentation by [William Chase](https://www.williamrchase.com/) on the Glamour of Graphics. The main takeaway is that in order to make aesthetically pleasing visualizations you should pay attention to 3 characteristics: (1) layout, (2) typography, and (3) color.

1.  **Layout**
    -   Appropriate use of *titles* can go a long way in decluttering your plots, it makes axis titles redundant and could aid with legend positioning or removal. See `ggtext` [package](https://github.com/wilkelab/ggtext)
    -   *Alignments* create symmetry and a seamless experience for redability. Align titles all the way to the start of the plot (including axis text). Avoid having people tilt their heads: rotate your plot!
    -   Mind your lines, question if background *grid lines* are neccessary.
    -   Don't be afraid of *whitespace*: less is more! If you cram in too much information, the message can get lost.
2.  **Typography**
    -   Make use of other *fonts*, more often than not built-in fonts are outdated. Download [free fonts](fonts.google.com)
    -   Maintain *hierarchy* (size, weight, color, spacing, contrasting typespace) to guide the reader.
    -   Avoid using oldstyle *numbering*, do use lining numbering in which the height is consistent. Similarly, avoid using proportional numbering, do use a tabular kind for width consistency.
3.  **Color**
    -   Think about *color theory*. When choosing hues, use color wheel to convey sentiments:
        -   complementary (high contrast)
        -   analogous (calm, harmonious)
        -   triadic (vibrant, contrast)
    -   Control colors by Hue, saturation, lightness/brightness (*HSB*).
    -   Create *your own palette* using an eyedropper tool (e.g. color slurp tool for Mac)
    -   Mind *accessibility*, think about color-blindness for example.
    -   For plot *backgrounds*, avoid completely white or black, pastel colors are a good option.

Using these principles I would like to see how songs get ranked across time, and which songs are ranked the highest.

Load libraries
--------------

``` r
library(tidyverse)
library(skimr)
library(patchwork)
library(ggpubr)
library(cowplot)
library(ggtext)
```

Get Data
--------

``` r
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')
```

Data structure
--------------

``` r
skim(polls)
```

|                                                  |       |
|:-------------------------------------------------|:------|
| Name                                             | polls |
| Number of rows                                   | 535   |
| Number of columns                                | 9     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 7     |
| numeric                                          | 2     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

**Variable type: character**

| skim\_variable   |  n\_missing|  complete\_rate|  min|  max|  empty|  n\_unique|  whitespace|
|:-----------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| title            |           0|            1.00|    2|   59|      0|        309|           0|
| artist           |           0|            1.00|    3|   56|      0|        207|           0|
| gender           |           0|            1.00|    4|    6|      0|          3|           0|
| critic\_name     |           0|            1.00|    6|   23|      0|        107|           0|
| critic\_rols     |           0|            1.00|    2|   28|      0|         48|           0|
| critic\_country  |           0|            1.00|    2|   18|      0|         13|           0|
| critic\_country2 |         530|            0.01|    9|    9|      0|          1|           0|

**Variable type: numeric**

| skim\_variable |  n\_missing|  complete\_rate|     mean|    sd|    p0|   p25|   p50|   p75|  p100| hist  |
|:---------------|-----------:|---------------:|--------:|-----:|-----:|-----:|-----:|-----:|-----:|:------|
| rank           |           0|               1|     3.00|  1.42|     1|     2|     3|     4|     5| ▇▇▇▇▇ |
| year           |           0|               1|  1998.15|  8.89|  1979|  1993|  1996|  2003|  2019| ▂▇▆▂▂ |

``` r
skim(rankings)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | rankings |
| Number of rows                                   | 311      |
| Number of columns                                | 12       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| character                                        | 3        |
| numeric                                          | 9        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

**Variable type: character**

| skim\_variable |  n\_missing|  complete\_rate|  min|  max|  empty|  n\_unique|  whitespace|
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| title          |           0|               1|    2|   59|      0|        309|           0|
| artist         |           0|               1|    3|   56|      0|        207|           0|
| gender         |           0|               1|    4|    6|      0|          3|           0|

**Variable type: numeric**

| skim\_variable |  n\_missing|  complete\_rate|     mean|     sd|    p0|     p25|   p50|     p75|  p100| hist  |
|:---------------|-----------:|---------------:|--------:|------:|-----:|-------:|-----:|-------:|-----:|:------|
| ID             |           0|               1|   156.00|  89.92|     1|    78.5|   156|   233.5|   311| ▇▇▇▇▇ |
| year           |           0|               1|  2000.41|   9.20|  1979|  1994.0|  1999|  2007.0|  2019| ▂▆▇▃▅ |
| points         |           0|               1|    10.32|  14.64|     2|     4.0|     6|    10.0|   140| ▇▁▁▁▁ |
| n              |           0|               1|     1.72|   2.01|     1|     1.0|     1|     2.0|    18| ▇▁▁▁▁ |
| n1             |           0|               1|     0.34|   0.89|     0|     0.0|     0|     0.0|     9| ▇▁▁▁▁ |
| n2             |           0|               1|     0.34|   0.69|     0|     0.0|     0|     1.0|     5| ▇▁▁▁▁ |
| n3             |           0|               1|     0.34|   0.59|     0|     0.0|     0|     1.0|     3| ▇▃▁▁▁ |
| n4             |           0|               1|     0.34|   0.62|     0|     0.0|     0|     1.0|     4| ▇▃▁▁▁ |
| n5             |           0|               1|     0.34|   0.60|     0|     0.0|     0|     1.0|     5| ▇▁▁▁▁ |

Data Wrangling
--------------

``` r
rank_freq<- rankings %>% 
  group_by(year) %>% 
  summarise(Freq= n())

rank_top<- rankings %>% 
  group_by(gender) %>% 
  top_n(1, points) %>% 
  mutate(label= paste(title, artist, year, sep = "\n"))
```

Visualizations
--------------

``` r
p1<-ggplot() +
  theme_minimal() +
  geom_bar(data= rank_freq, aes(year, Freq), stat = "identity") +
  geom_bar(data= rank_freq %>% filter(year>=1990 & year<=2000), aes(year, Freq), stat = "identity", fill = "gold") +
  geom_vline(xintercept = 1990) +
  geom_vline(xintercept = 2000) +
  geom_bracket(xmin = 1990, xmax = 2000, y.position = 30,
    label = "The golden era of Hip-hop") +
  ggtitle("Number of songs ranked for each year: the 90's don't disappoint") +
  theme(plot.title.position = "plot", axis.title = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


p2<- ggplot() +
  theme_minimal() +
  geom_point(data= rankings, aes(year, points, color=gender), alpha=0.8, size=2)  +
  geom_vline(xintercept = 1990) +
  geom_vline(xintercept = 2000) +
  theme(legend.position = "none") +
  ylab("Points") +
  geom_point(data= rankings, aes(year, points, color=gender), alpha=0.8, size=2, shape= 1, color="black") +
  geom_curve(data= rank_top, aes(xend=year , yend= points, x = c(2012, 2005, 2010), y=points + 20),
             curvature = .2, arrow = arrow(length = unit(1, "mm")), color="gold") +
  geom_label(data=rank_top, aes(x=c(2012, 2005, 2010), points + 20, label=label, fill=gender), color= "gold", fontface = "bold") +
  scale_color_manual(values=c("grey", "black", "white")) +
  scale_fill_manual(values=c("grey", "black", "white")) +
  labs(title = "Number of points for songs by <strong>male</strong></b>, 
    <strong><span style='color:#808080'>female</span></strong></b>, and <strong><span style='color:#FFFFFF'>mixed artists</span></strong></b> by year") +
  theme(plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "gold"
    ), 
        plot.title.position = "plot", axis.title = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

p1
```

![](TidyTuesday_15_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
p2
```

![](TidyTuesday_15_files/figure-markdown_github/unnamed-chunk-1-2.png)
