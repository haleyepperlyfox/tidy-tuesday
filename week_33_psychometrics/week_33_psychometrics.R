# Tidy Tuesday Week 33 - Psychometrics with TV characters

# load libraries
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(patchwork)
library(ggpubr)
library(png)
library(here)


# read in data
tuesdata <- tidytuesdayR::tt_load(2022, week = 33)
psych <- tuesdata$psych_stats

# create list of characters and attributes of interest
characters <- c("Andy Bernard", "Dwight Schrute", "Jim Halpert", "Phyllis Lapin",
                "Ryan Howard", "Stanley Hudson")

attribute <- c("extrovert/introvert", "awkward/charming", "driven/unambitious",
              "charismatic/uninspiring", "confident/insecure", "competitive/cooperative",
              "funny/humorless")

# filter to the office sales staff and the desired characters and attributes
office <- psych %>% 
  filter(uni_name == "The Office" & char_name %in% characters & question %in% attribute) 

# add column for most commonly selected personality trait and average rating
# and column for the other personality trait and that rating (1 - previous rating)
office_1 <- office %>% 
  select(-c(rank, rating_sd, number_ratings)) %>% 
  separate(question, c("option_1", "option_2")) %>% 
  rename(trait_selected = personality, trait_selected_rating = avg_rating) %>% 
  mutate(trait_not_selected = case_when(option_1 == trait_selected ~ option_2,
                                        TRUE ~ option_1)) %>% 
  mutate(trait_not_selected_rating = (100 - trait_selected_rating))

# make plot
showtext_auto(enable = TRUE) 

plot_list = list()
for (character in characters){
p <- ggplot(office_1 %>% filter(char_name==character), aes(x = reorder(trait_selected, trait_selected_rating), y = trait_selected_rating)) +
  geom_bar(stat = "identity", width = 0.75, fill = "deepskyblue3") +
  labs(y = NULL, x = NULL) +
  geom_text(aes(label = paste0(round(trait_selected_rating, digits = 0), "%")), hjust = -0.2, color = "white", size = 10) +
  geom_text(aes(label = trait_selected), color= "black", hjust = 1.1, size=10) +
  scale_y_reverse(limits = c(100, 0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
plot_list[[character]] = p
print(p)
}


plot_list_2 = list()
for (character in characters){
p2 <- ggplot(office_1 %>% filter(char_name==character), aes(x = reorder(trait_not_selected, -trait_not_selected_rating), y = trait_not_selected_rating)) +
  geom_bar(stat = "identity", width = 0.75, fill = "darkorange3") +
  labs(y = NULL, x = NULL) +
  geom_text(aes(label = paste0(round(trait_not_selected_rating, digits = 0), "%")), hjust = 1.1, color = "white", size = 10) +
  geom_text(aes(label = trait_not_selected), color= "black", hjust = -0.1, size=10) + 
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(position = "top") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
plot_list_2[[character]] = p2
print(p2)
}

# put all of the salespeople plots together with patchwork and close gap between plots
p1 <- plot_list$`Jim Halpert` + plot_spacer() + plot_list_2$`Jim Halpert` + plot_layout(widths = c(4, -0.5, 4))
p2 <- plot_list$`Andy Bernard` + plot_spacer() + plot_list_2$`Andy Bernard` + plot_layout(widths = c(4, -0.5, 4))
p3 <- plot_list$`Phyllis Lapin` + plot_spacer() + plot_list_2$`Phyllis Lapin` + plot_layout(widths = c(4, -0.5, 4))
p4 <- plot_list$`Dwight Schrute` + plot_spacer() + plot_list_2$`Dwight Schrute` + plot_layout(widths = c(4, -0.5, 4))
p5 <- plot_list$`Ryan Howard` + plot_spacer() + plot_list_2$`Ryan Howard` + plot_layout(widths = c(4, -0.5, 4))
p6 <- plot_list$`Stanley Hudson` + plot_spacer() + plot_list_2$`Stanley Hudson` + plot_layout(widths = c(4, -0.5, 4))

# arrange the first row of 3 plots
arrange2 <- ggarrange(
  p1, ggplot() + theme_void(), p2, ggplot() + theme_void(), p3, 
  ncol = 5, nrow = 2,
  widths = c(1, -0.15, 1, -0.15, 1),
  labels = c("Jim Halpert", "", "Andy Bernard", "", "Phyllis Lapin"),
  font.label = list(size = 40),
  hjust = -2
)

# arrange the second row of 3 plots
arrange3 <- ggarrange(
  p6, ggplot() + theme_void(), p5, ggplot() + theme_void(), p4,
  ncol = 5, nrow = 2,
  widths = c(1, -0.15, 1, -0.15, 1),
  labels = c("Stanley Hudson", "", "Ryan Howard", "", "Dwight Schrute"),
  font.label = list(size = 40),
  hjust = -2
)

# combine two rows into one plot and save
arrange4 <- ggarrange(arrange2, ggplot() + theme_void(), arrange3,
                      nrow = 3,
                      heights = c(1, -0.5, 1))

ggsave("week_33_psychometrics/office_plot.png", arrange4, width = 33, height = 15)



# that plot above with all of the salespeople  doesn't make for a great twitter post, 
# so let's also create a smaller version with just Jim and Dwight and add in their photos

# add a picture of Jim and Dwight - download photos locally using image links and save as png
image_links <- tuesdata$characters
image_links_1 <- image_links %>% 
  filter(name == "Jim Halpert" | name == "Dwight Schrute")

d_image <- readPNG(here("week_33_psychometrics/dwight_photo.png"), native = TRUE)
j_image <- readPNG(here("week_33_psychometrics/jim_photo.png"), native = TRUE)

dwight <- p4 + labs(title = "How do people rate The Office's Dwight and Jim on sales-related personality traits?",
            subtitle = "Dwight Schrute") +
  theme(
    plot.title = element_text(size=45, hjust=3, face="bold"),
    plot.subtitle = element_text(size=30, hjust=-0.9, face="bold")
  ) + 
  inset_element(p = d_image,
                left = 0.5,
                bottom = 0.35,
                right = 1,
                top = 1)

jim <- p1  + labs(subtitle = "Jim Halpert",
                  caption = "Haley Fox | Tidy Tuesday Week 33 | Data: Open-Source Psychometrics Project courtesy of Tanya Shapiro") +
  theme(
    plot.subtitle = element_text(size=30, hjust=-0.9, face="bold"),
    plot.caption = element_text(hjust=-1, size=20, face="bold")
  ) + 
  inset_element(p = j_image,
                left = 0.5,
                bottom = 0.35,
                right = 1,
                top = 1)


arrange_jim_dwight <- ggarrange(
  dwight, jim,
  nrow = 2,
  font.label = list(size = 40)
)

ggsave("week_33_psychometrics/office_plot_jim_dwight.png", arrange_jim_dwight, width = 13, height = 7)
