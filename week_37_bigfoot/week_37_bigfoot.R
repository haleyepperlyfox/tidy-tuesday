# Tidy Tuesday Week 37 - Bigfoot data from Bigfoot Field Researchers Organization (BFRO) by way of Data.World.
# following this logistic regression tutorial: https://stats.oarc.ucla.edu/r/dae/logit-regression/

# load libraries
library(tidyverse)
library(tidytuesdayR)
library(multcomp)
library(PrettyCols)

# read in data
tuesdata <- tidytuesdayR::tt_load('2022-09-13')
bigfoot <- tuesdata$bigfoot

# see which states have the most sightings
bigfoot %>% 
  group_by(state) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

# subset to these 4 states of interest
bf_state <- bigfoot %>% 
  filter(state == "California" | state == "Ohio" | state == "Texas" | state == "Michigan")

# subset to only class A and B
bf_a_b <- bf_state %>% 
  filter(classification == "Class A" | classification == "Class B")

# bring out year from date column
bf_yr <- bf_a_b %>% 
  mutate(year = lubridate::year(date))

# subset to columns of interest for logistic regression
bf_sub <- bf_yr %>% 
  dplyr::select(classification, state, season, year, temperature_mid, cloud_cover, visibility) %>% 
  rename(State = state)

# there are many NAs in the data, but let's roll with it for now
# these rows will be removed in the logistic regression
summary(bf_sub)

# 2-way contingency tables - we want to make sure there are no cells with 0
# do this for categorical predictor variables
xtabs(~classification + State, data = bf_sub)
xtabs(~classification + season, data = bf_sub)

# remove the unknown categories for season
bf_2 <- bf_sub %>% 
  filter(!season == "Unknown")

# convert classification to binary (Class A = 1, Class B = 0)
bf_3 <- bf_2 %>% 
  mutate(class = case_when(classification == "Class A" ~ 1,
                           TRUE ~ 0))

# convert season and state to factors
bf_3$State <- factor(bf_3$State)
bf_3$season <- factor(bf_3$season)



# logistic regression for predicting whether the experience is class A or B
model <- glm(class ~ State + season + 
               year + temperature_mid + cloud_cover + visibility, 
             data = bf_3, family = "binomial")
summary(model)

# Tukey's pairwise comparison for states - significant differences by states
summary(glht(model, mcp(State = "Tukey")))

# Tukey's pairwise comparison for seasons - no significant differences by seasons
summary(glht(model, mcp(season = "Tukey")))



# predicted probabilities for a range of visibility values across 4 states

# determine the range for visibility
summary(bf_3)

# make a new dataframe with the mean for all values except state and visibility
newdata2 <- with(bf_3, data.frame(season = "Summer", 
                                  year = round(mean(year, na.rm = TRUE), digits = 0), 
                                  temperature_mid = mean(temperature_mid, na.rm = TRUE),
                                  cloud_cover = mean(cloud_cover, na.rm = TRUE),
                                  State = c("California", "Ohio", "Michigan", "Texas"), 
                                  visibility = rep(seq(from = 0.8, to = 10, length.out = 100), 4)))

# specify factors for state
newdata2$State <- factor(newdata2$State, levels = c("Texas", "Ohio", "Michigan", "California"))

# predict the probability for class A or B based on range of states and visibility values
newdata3 <- cbind(newdata2, predict(model, newdata = newdata2, type = "link", se = TRUE))

# calculate 95% confidence intervals
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# plot predicted probabilities
ggplot(newdata3, aes(x = visibility, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL,  ymax = UL, fill = State), alpha = 0.2) + 
  geom_line(aes(colour = State), size = 1) +
  scale_y_continuous(limits=c(0, 0.8)) +
  theme_classic() +
  labs(x = "Visibility", 
       y = "Predicted probability Class A",
       title = "Are bigfoot encounters in lower visibility less reliable?",
       subtitle = "Probability that a bigfoot encounter is classified as Class A (more reliable) vs. Class B \n(less reliable) increases with increasing visibility across states.",
       caption = "Haley Fox | Tidy Tuesday Week 37 | Data: Bigfoot Field Researchers Organization (BFRO) by way of Data.World.") +
  theme(plot.caption = element_text(hjust=.3, face="bold")) +
  annotate("text", x = 3.8, y = 0.775, label ='class ~ state + season + year + temperature + cloud cover + visibility', size = 2.5, fontface = "bold") +
  annotate("text", x = 3.85, y = 0.75, label ='state, year, and visibility are significant predictors of class at p < 0.05', size = 2.5, fontface = "bold") +
  scale_colour_pretty_d("Dark") +
  scale_fill_pretty_d("Dark")

# save plot  
ggsave("week_37_bigfoot/bigfoot.png", height=5, width=7)
