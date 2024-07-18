install.packages("tidyverse")
library("tidyverse")
install.packages("palmerpenguins")
library(palmerpenguins)

View(penguins)

str(penguins)

penguins_by_species <-
  penguins %>% group_by(species) %>% drop_na() %>% summarise(mean_bill_length=mean(bill_length_mm), mean_bill_depth=mean(bill_depth_mm), mean_flipper_length=mean(flipper_length_mm), mean_body_mass=mean(body_mass_g))
penguins_by_species

penguins_by_island <- 
  penguins %>% group_by(island) %>% drop_na() %>% summarise(mean_bill_length=mean(bill_length_mm), mean_bill_depth=mean(bill_depth_mm), mean_flipper_length=mean(flipper_length_mm), mean_body_mass=mean(body_mass_g))
penguins_by_island

penguins_by_sex <- 
  penguins %>% group_by(sex) %>% drop_na() %>% summarise(mean_bill_length=mean(bill_length_mm), mean_bill_depth=mean(bill_depth_mm), mean_flipper_length=mean(flipper_length_mm), mean_body_mass=mean(body_mass_g))
penguins_by_sex

p1 <- penguins %>% drop_na()

ggplot(penguins, mapping = aes(x=body_mass_g, y=bill_length_mm, colour = species)) + geom_point() + facet_wrap(~species) + labs(title = "Body Mass Vs. Bill Length amongst different speices")

ggplot(p1, mapping = aes(x=body_mass_g, y=bill_length_mm, colour = sex)) + geom_point() + facet_wrap(~sex) + labs(title = "Body Mass Vs. Bill Length amongst different genders")

ggplot(penguins, mapping = aes(x=body_mass_g, y=bill_length_mm, colour = island)) + geom_point() + facet_wrap(~island) + labs(title = "Body Mass Vs.  Bill length in different islands")


ggplot(penguins, mapping = aes(x=body_mass_g, y=bill_depth_mm, colour = species)) + geom_point() + facet_wrap(~species) + labs(title = "Body Mass Vs. Bill Depth amongst different speices")

ggplot(p1, mapping = aes(x=body_mass_g, y=bill_depth_mm, colour = sex)) + geom_point() + facet_wrap(~sex) + labs(title = "Body Mass Vs. Bill Depth amongst different genders")

ggplot(penguins, mapping = aes(x=body_mass_g, y=bill_depth_mm, colour = island)) + geom_point() + facet_wrap(~island) + labs(title = "Body Mss Vs. Bill Depth in different islands")


ggplot(penguins, mapping = aes(x=body_mass_g, y=flipper_length_mm, colour = species)) + geom_point() + facet_wrap(~species) + labs(title = "Body Mass Vs. Flipper Length amongst different speices")

ggplot(p1, mapping = aes(x=body_mass_g, y=flipper_length_mm, colour = sex)) + geom_point() + facet_wrap(~sex) + labs(title = "Body Mass Vs. Flipper Length amongst different genders")

ggplot(penguins, mapping = aes(x=body_mass_g, y=flipper_length_mm, colour = island)) + geom_point() + facet_wrap(~island) + labs(title = "Body Mass Vs. Flipper length in differen islands")


ggplot(penguins, mapping = aes(x=flipper_length_mm, y=bill_length_mm, colour = species)) + geom_point() + geom_smooth(method = lm, se = FALSE, color="black") + facet_wrap(~species) + labs(title = "Flipper Length Vs. Bill Length amongst different Species")

ggplot(p1, mapping = aes(x=flipper_length_mm, y=bill_length_mm, colour = sex)) + geom_point() + geom_smooth(method = lm, se = FALSE, color="black") + facet_wrap(~sex) + labs(title = "Flipper Length Vs. Bill Length amongst different genders")

ggplot(penguins, mapping = aes(x=flipper_length_mm, y=bill_length_mm, colour = island)) + geom_point() + geom_smooth(method = lm, se = FALSE, color="black") + facet_wrap(~island) + labs(title = "Flipper Length Vs. Bill Length in different islands")


ggplot(penguins, mapping = aes(x=flipper_length_mm, y=bill_depth_mm, colour = species)) + geom_point() + geom_smooth(method = lm, se = FALSE, color="black") + facet_wrap(~species) + labs(title = "Flipper Length Vs. Bill Depth amongst different Species")

ggplot(p1, mapping = aes(x=flipper_length_mm, y=bill_depth_mm, colour = sex)) + geom_point() + geom_smooth(method = lm, se = FALSE, color="black") + facet_wrap(~sex) + labs(title = "Flipper Length Vs. Bill Depth amongst different genders")

ggplot(penguins, mapping = aes(x=flipper_length_mm, y=bill_depth_mm, colour = island)) + geom_point() + geom_smooth(method = lm, se = FALSE, color="black") + facet_wrap(~island) + labs(title = "Flipper Length Vs. Bill Depth in different islands")


ggplot(penguins, aes(x=island, fill = species)) + geom_bar()

ggplot(p1, aes(x=island, fill = sex)) + geom_bar()

ggplot(penguins, aes(x=island, fill = factor(year))) + geom_bar()


ggplot(penguins, mapping = aes(x=species, fill = species)) + geom_bar() + labs(title = "Number of Penguins of each Species in the dataset")

ggplot(penguins, mapping = aes(x=island, fill = island)) + geom_bar() + labs(title = "Number of Penguins on each island")

ggplot(p1, mapping = aes(x=sex, fill = sex)) + geom_bar() + labs(title = "Number of Different Genders amongst the entire Penguin populace in Palmer Archipelago")

ggplot(penguins, mapping = aes(x=year, fill = factor(year))) + geom_bar() + labs(title = "Number of Penguins found each year", fill="Year")


data <- penguins
data <- na.omit(data)
summary_data <- data %>%
  group_by(island, species) %>%
  summarise(mean_flipper_length = mean(flipper_length_mm), mean_bill_length = mean(bill_length_mm), mean_bill_depth = mean(bill_depth_mm), .groups = 'drop')

melted_summary_data <- summary_data %>%
  pivot_longer(cols = mean_flipper_length, names_to = "attribute", values_to = "value")

ggplot(melted_summary_data, aes(x = island, y = species, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Mean Flipper Length (mm) by Island and Species",
       x = "Island",
       y = "Species",
       fill = "Mean Flipper Length (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

melted_summary_data <- summary_data %>%
  pivot_longer(cols = mean_bill_length, names_to = "attribute", values_to = "value")

ggplot(melted_summary_data, aes(x = island, y = species, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Mean Bill Length (mm) by Island and Species",
       x = "Island",
       y = "Species",
       fill = "Mean Bill Length (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

melted_summary_data <- summary_data %>%
  pivot_longer(cols = mean_bill_depth, names_to = "attribute", values_to = "value")

ggplot(melted_summary_data, aes(x = island, y = species, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Mean Bill Depth (mm) by Island and Species",
       x = "Island",
       y = "Species",
       fill = "Mean Bill Depth (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
