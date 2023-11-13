install.packages(c("tidyverse", "sf", "emo", "janitor", "palmerpenguins", "usethis", "lubridate"))
library(dplyr)
library(palmerpenguins)
library(tidyverse)

data(penguins)
head(penguins, n=20)
glimpse(penguins)

##Question 1
filtered_penguins <- penguins %>% filter(!is.na(sex)) %>% select(species, island, bill_length_mm, body_mass_g)
filtered_penguins

##Question 2
num_penguins <- penguins %>% count()
num_penguins
num_speices <- penguins %>% count(species)
num_speices
num_island <- penguins %>% count(island)
num_island
num_speices.island <- penguins %>% count(species, island)
num_speices.island

##Question 3
overall_mean_mass <- penguins %>% summarize(mean_body_mass = mean(body_mass_g, na.rm = TRUE))
overall_mean_mass
species_mean_mass <- penguins %>% group_by(species) %>% summarize(mean_body_mass = mean(body_mass_g, na.rm = TRUE))
species_mean_mass
species_mean_traits <- penguins %>% group_by(species) %>% summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE), mean_body_mass = mean(body_mass_g, na.rm = TRUE))
species_mean_traits


# Data Visualization
# Q1
penguins %>%
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Body Mass (g)", y = "Count") +
  ggtitle("Histogram of Penguin Body Mass")


penguins %>%
  ggplot(aes(x = body_mass_g, fill = species)) +
  geom_histogram(binwidth = 100, color = "black") +
  labs(x = "Body Mass (g)", y = "Count") +
  ggtitle("Histogram of Penguin Body Mass by Species") +
  scale_fill_brewer(palette = "Set3")

custom_colors <- c("darkorange", "purple", "cyan4")

# Create a histogram of body mass with custom fill colors
penguins %>%
  ggplot(aes(x = body_mass_g, fill = species)) +
  geom_histogram(binwidth = 100, color = "black") +
  labs(x = "Body Mass (g)", y = "Count") +
  ggtitle("Histogram of Penguin Body Mass by Species") +
  scale_fill_manual(values = custom_colors)


# Define custom fill colors
custom_colors <- c("darkorange", "purple", "cyan4")

# Create a histogram of body mass with custom fill colors and transparency
penguins %>%
  ggplot(aes(x = body_mass_g, fill = species)) +
  geom_histogram(binwidth = 100, color = "black", alpha = 0.3) +  # Adjust transparency here
  labs(x = "Body Mass (g)", y = "Count") +
  ggtitle("Histogram of Penguin Body Mass by Species") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()


# Q2
penguins %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  labs(
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    title = "Scatter Plot of Body Mass vs. Flipper Length"
  ) +
  theme_minimal()

penguins %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, shape = species)) +
  geom_point(size = 3, alpha = 0.7) +
 # Define different point shapes for each species
  labs(
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    title = "Scatter Plot of Body Mass vs. Flipper Length by Species"
  ) +
  theme_minimal()

penguins %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +  # Define different colors for each species
  labs(
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    title = "Scatter Plot of Body Mass vs. Flipper Length by Species"
  ) +
  theme_minimal()

penguins %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = species)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +  # Define different colors for each species
  # Define different point sizes for each species
  labs(
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    title = "Scatter Plot of Body Mass vs. Flipper Length by Species"
  ) +
  theme_minimal()





