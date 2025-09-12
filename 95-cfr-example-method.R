# Load library
library(ggplot2)

# Example data
df <- data.frame(
  Time = factor(c(0, 1, 2, 3)),
  Incidence = c(5, 7, 4, 3)
)

# Plot
ggplot(df, aes(x = Time, y = Incidence)) +
  geom_bar(stat = "identity", fill = "grey30") +
  labs(x = "Time (days)", y = "Incidence") +
  theme_classic()



# next -------------------------------------------------------------------

# Load library
library(ggplot2)

# Example data
df2 <- data.frame(
  Time = factor(c(0, 1, 2, 3)),
  SerialInterval = c(0.2, 0.4, 0.3, 0.1)
)

# Plot
ggplot(df2, aes(x = Time, y = SerialInterval)) +
  geom_bar(stat = "identity", fill = "grey30") +
  labs(x = "Time (days)", y = "Onset to Death") +
  theme_classic()



# next -------------------------------------------------------------------

# Load library
library(ggplot2)

# Example data
df2 <- data.frame(
  Time = factor(c(0, 1, 2, 3)),
  SerialInterval = c(1, 1, 1, 1)
)

# Plot
ggplot(df2, aes(x = Time, y = SerialInterval)) +
  geom_bar(stat = "identity", fill = "grey30") +
  labs(x = "Time (days)", y = "Deaths") +
  theme_classic() + ylim(0,7)
