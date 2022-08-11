# Tidy Tuesday Week 32 - Ferris Wheels

# Read in with tidytuesdayR package 
library(tidytuesdayR)
library(tidyverse)
library(ggsvg)
library(showtext)

data <- tidytuesdayR::tt_load('2022-08-09')

# convert from list to dataframe
df <- do.call(rbind.data.frame, data)
colnames(df)

# remove rows with NAs in variables of interest
df_1 <- df %>% 
  filter(!construction_cost == "Unknown") %>% 
  filter(!is.na(ticket_cost_to_ride)) %>% 
  filter(!is.na(hourly_capacity)) %>% 
  filter(!ticket_cost_to_ride == "TBA")

# clean up construction cost column
df_2 <- df_1 %>% 
  mutate(construction_cost = parse_number(construction_cost)) %>% 
  mutate(construction_cost = (construction_cost*1000000))
         
# clean up ticket cost to ride column - assume all adults
df_3 <- df_2 %>% 
  mutate(ticket_cost_to_ride = parse_number(ticket_cost_to_ride))

# calculate amount a ferris wheel can make in a day assuming operating 
# for 12 hours at 100% capacity - and then how many days it will take to recoup construction costs
#high assumptions to overestimate how quickly they'll pay it off
df_3 <- df_3 %>% 
  mutate(twelve_hour_capacity = hourly_capacity*12) %>% 
  mutate(day_sales = twelve_hour_capacity*ticket_cost_to_ride) %>% 
  mutate(days_till_paid_off = construction_cost/day_sales)

# rename Eye on Malaysia (1)
df_3$name <- ifelse(df_3$name == "Eye on Malaysia (1)", "Eye on Malaysia", df_3$name)

# lollipop chart
svg_url <- "https://www.svgrepo.com/download/89559/cable-car-cabin.svg"
svg_txt <- paste(readLines(svg_url), collapse = "\n")
showtext_auto(enable = TRUE) 

df_3 %>%
  ggplot(aes(reorder(name, days_till_paid_off), days_till_paid_off)) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray60") +
  geom_hline(yintercept=200, linetype="dashed", color = "gray60") +
  geom_hline(yintercept=400, linetype="dashed", color = "gray60") +
  geom_hline(yintercept=600, linetype="dashed", color = "gray60") +
  geom_point() +
  geom_segment(aes(x=name, xend=name, y=0, yend=days_till_paid_off), color = 'black',size = 1) +
  geom_point_svg(aes(name, days_till_paid_off), svg = svg_txt, size = 5.5) +
  coord_flip() +
  theme_minimal() +
  theme(plot.background  = element_rect(fill="lavender")) +
  theme(panel.grid.major = element_line(colour="gray")) +
  labs(title = "How many days does it take to recoup construction costs of a ferris wheel?",
       subtitle = "Assuming operating for 12 hours per day, 7 days a week, at 100% adult capacity",
       caption = "Haley Fox |  Tidy Tuesday Week 32 | Data: @Emil_Hvitfeldt {ferriswheels}",
       y = "Days",
       x = "") +
  theme(
    axis.text.x = element_text(size=20, color="midnightblue"),
    axis.text.y = element_text(size=20, color="midnightblue", margin = margin(r = -17)),
    axis.title =  element_text(size=20, color="midnightblue"),
    plot.title = element_text(size=30, color="midnightblue", hjust=.5, face="bold"),
    plot.subtitle = element_text(size=22, color="midnightblue", hjust=.5),
    plot.caption = element_text(hjust=.5, size=20, color="midnightblue", face="bold"),
    panel.grid.major = element_blank()
  )

ggsave("week_32_ferris_wheels/tidytuesday_week_32.png",
       height = 5,
       width = 6,
       dpi=320)

