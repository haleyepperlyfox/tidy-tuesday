# Tidy Tuesday Week 36 - Lego data from Rebrickable.com

# load libraries
library(tidyverse)
library(tidytuesdayR)
library(PrettyCols)
library(png)
library(patchwork)

# read in data
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

# view available palettes from PrettyCols package
view_all_palettes()

# read in images saved as pngs
image_1949 <- readPNG("week_36_legos/images/700.2-1.png", native = TRUE)
image_1980 <- readPNG("week_36_legos/images/7740-1.png", native = TRUE)
image_2015 <- readPNG("week_36_legos/images/bigbox-1.png", native = TRUE)

# plot
plot <- ggplot(sets, aes(x = year, y = num_parts)) +
  geom_point(aes(color = num_parts)) +
  scale_colour_pretty_c("Dark") +
  geom_smooth(color = "orange", lwd = 0.5) +
  geom_segment(aes(x = 1949, xend = 1949, y = 300, yend = 9000), color = "black", lwd = 0.25) +
  geom_segment(aes(x = 1980, xend = 1980, y = 910, yend = 5000), color = "black", lwd = 0.25) +
  geom_segment(aes(x = 2005, xend = 2014.7, y = 9987, yend = 9987), color = "black", lwd = 0.25) +
  labs(y = "Number of parts", 
       x = "Year", 
       title = "Are the number of pieces in Lego sets increasing?",
       subtitle = "Visualizing the trend in number of Lego parts over time, we see a slight increase in the \naverage number of parts per set with a larger increase in spread.",
       caption = "Haley Fox | Tidy Tuesday Week 36 | Data: Rebrickable.com courtesy of Georgios Karamanis") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title=element_text(color="#47577A", size=16, face = "bold"),
        plot.caption=element_text(color="#47577A", size=10, hjust=0),
        plot.subtitle=element_text(color="#47577A",size=10)) +  
  annotate("text", x = 2000.5, y = 11500, label ='The Ultimate Battle for Chima',
           colour = "#47577A", size = 3.5, fontface = "bold") +  
  annotate("text", x = 2005, y = 7400, label ='9,987 parts',
           colour = "#47577A", size = 3.5, fontface = "bold") +  
  annotate("text", x = 1980, y = 7600, label ='Inter-City Passenger Train',
           colour = "#47577A", size = 3.5, fontface = "bold") + 
  annotate("text", x = 1984.5, y = 2850, label ='786 parts',
           colour = "#47577A", size = 3.5, fontface = "bold") +  
  annotate("text", x = 1956, y = 11000, label ='Large Gift Set (ABB)',
           colour = "#47577A", size = 3.5, fontface = "bold") +  
  annotate("text", x = 1960, y = 7400, label ='178 parts',
           colour = "#47577A", size = 3.5, fontface = "bold")

# add images to plot
plot + inset_element(p = image_1949, 0.02, 0.65, 0.25, 0.87) +     
  inset_element(p = image_1980, 0.28, 0.3, 0.58, 0.6) +     
  inset_element(p = image_2015, 0.58, 0.64, 0.8, 0.92)

ggsave("week_36_legos/legos.png", height=5, width=7)

