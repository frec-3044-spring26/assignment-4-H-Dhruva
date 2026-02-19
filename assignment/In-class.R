library(ratdat)
library(tidyverse)

## DAY 1

## GGPLOT method introductions

ggplot(data = complete_old, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.2, color = "blue")

ggplot(data = complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) +
  geom_point(alpha = 0.2) +
  scale_color_viridis_d()

ggplot(data = complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10()

ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length, fill = plot_type)) +
  geom_jitter(aes(color = plot_type), size = 2, alpha = 0.1) +
  geom_boxplot() +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14, color = "navy"), legend.position = "none") +
  labs(title = "Rodent Size by plot type",
       x = "Plot type",
       y = "Hindfoot length (mm)") +
  facet_wrap(vars(sex), ncol = 1)

ggplot(data = complete_old, mapping = aes(x = plot_type, y = hindfoot_length, fill = plot_type)) +
  geom_jitter(aes(color = plot_type), size = 2, alpha = 0.1) +
  geom_boxplot() +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14, color = "navy"), legend.position = "none") +
  labs(title = "Rodent Size by plot type",
       x = "Plot type",
       y = "Hindfoot length (mm)") +
  facet_wrap(vars(sex), nrow = 1)

species_count <- complete_old |>
  group_by(species) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  slice(1:5)

complete_old |>
  filter(species %in% species_count$species)  |>
  ggplot(mapping = aes(x = plot_type, y = hindfoot_length, fill = plot_type)) +
  geom_jitter(aes(color = plot_type), size = 2, alpha = 0.1) +
  geom_boxplot() +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14, color = "navy"), legend.position = "none") +
  labs(title = "Rodent Size by plot type",
       x = "Plot type",
       y = "Hindfoot length (mm)") +
  facet_grid(rows = vars(sex), cols = vars(species))


## DAY 2

complete_old |>
  # Chars are good grouping variables
  # group_by(species_id, plot_type) |>
  summarize(hindfoot_mean = mean(hindfoot_length, na.rm = TRUE),
            hindfoot_max = max(hindfoot_length, na.rm = TRUE),
            count = n(),
            # Works as group by
            .by = c("species", "plot_type"))

my_var <- "test_var"

# Set vector (ALL elements have to share data type)
my_vector <- c(2,6,12,24,32)

# Access individual element
my_vector[3]

# Access multiple elements
my_vector[c(1,5)]
my_vector[1:4]

# Loop-based access
i <- 2
my_vector[i]
my_vector[i] <- my_vector[i] * 10
my_vector * 10

# Matrix columns need to be same type
my_matrix <- matrix(data = 100, nrow = 3, ncol = 4)
my_matrix[2,3] <- my_matrix[2,3] / 10
my_matrix[2,3]
my_matrix[8]

# Return a column
my_matrix[,3]

# Return a row
my_matrix[3,]

my_matrix[,]
my_matrix[]

i <- 3
my_matrix[i,2]
my_matrix <- my_matrix * 52

# Need to share the same data type (can be as many dimensions as wanted)
array(100, dim = c(2,3,4))
my_array <- array(100, dim = c(2,3,4))

my_array[2,1,3] <- my_array[2,1,3] / 10

complete_old[1,]
