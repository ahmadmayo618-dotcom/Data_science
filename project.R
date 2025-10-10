
library(ggplot2)
datasets::trees
head(trees))
# 1. Scatter plot!
ggplot(trees, aes(x = Girth, y = Volume)) +
  geom_point(color = "black", size = 3) +
  labs(title = "Scatter Plot: Girth vs Volume") +
  theme_minimal()


# 2. Line plot
ggplot(trees, aes(x = Height, y = Volume)) +
  geom_line(color = "red") +
  geom_point(color = "blue") +
  labs(title = "Line Plot: Height vs Volume")
# 3. Bar plot (Average Volume by Height groups)
ggplot(trees, aes(x = factor(Height), y = Volume)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Bar Plot: Avg Volume by Height")


