library(dplyr)
library(ggplot2)

# Load dataset
data("mtcars")

# Convert rownames to a column
mtcars <- mtcars %>%
  mutate(car = rownames(mtcars))

# Create categorical variables
mtcars <- mtcars %>%
  mutate(
    cyl = factor(cyl),
    gear = factor(gear),
    efficiency = ifelse(mpg > median(mpg), "High", "Low")
  )

# Summary statistics by cylinders
summary_table <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    avg_mpg = mean(mpg),
    avg_hp = mean(hp),
    count = n()
  )

print(summary_table)

# Filter and arrange
filtered <- mtcars %>%
  filter(hp > 100) %>%
  arrange(desc(mpg))

print(filtered)

# Plot
ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "change title again",
    x = "Weight",
    y = "Miles per Gallon"
  ) +
  theme_minimal()


