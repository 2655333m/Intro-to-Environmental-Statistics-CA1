library(ggplot2)
library(readr)
library(readxl)
library(car)

data <- read.csv("data.csv")
file.info("data.csv")
data <- read_excel("data.csv")

#Subset to just Indian & Pacific
bleach_subset <- subset(data, Ocean_Name %in% c("Indian", "Pacific")) 
table(bleach$Ocean_Name)

#Clean the data (remove NAs and ensure Percent_Bleaching is numeric)
bleach_subset$Percent_Bleaching <- as.numeric(bleach_subset$Percent_Bleaching)
bleach <- na.omit(bleach_subset)

#Inspect Data
summary(bleach$Percent_Bleaching)
sd(bleach$Percent_Bleaching)

#Boxplot (Square-Root scale for skew correction)
ggplot(bleach, aes(x = Ocean_Name, y = Percent_Bleaching, fill = Ocean_Name)) +
  geom_boxplot(alpha = 0.8, width = 0.6, outlier.shape = 21, outlier.fill = "white") +
  scale_y_continuous(trans = "sqrt",  
                     breaks = c(0, 1, 5, 10, 25, 50, 100)) +
  labs(title = "Coral Bleaching by Ocean Basin",
       x = "Ocean Basin",
       y = "√(Bleaching %)",
       caption = "Data source: (van Woesik & Kratochwill 2022)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

#Q-Q Plot of untransformed
qqnorm(bleach$Percent_Bleaching)
qqline(bleach$Percent_Bleaching, col="red")

# Log-transform Q–Q plot 
qqnorm(log10(bleach$Percent_Bleaching + 1),
       main = "Log Q–Q Plot of Percent Bleaching (+1 adjusted)")
qqline(log10(bleach$Percent_Bleaching + 1),
       col = "red", lwd = 2)

#Visual Histogram plot to test Normality
ggplot(bleach, aes(x = Percent_Bleaching)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(title = "Histogram of Coral Bleaching (%) — Pacific and Indian Oceans",
       x = "Percent Bleaching", y = "Frequency") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.line = element_line(colour = "black", linewidth = 0.5),
  )

#Log10 Transformed Histogram
ggplot(bleach, aes(x = log10(Percent_Bleaching + 1))) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(
    title = "Histogram of Log(10)-Transformed Coral Bleaching (%) — Pacific and Indian Oceans",
    x = "log(10)(Percent Bleaching + 1)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.line = element_line(colour = "black", linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#Square-Root Transformed Histogram
ggplot(bleach, aes(x = sqrt(Percent_Bleaching + 1))) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(
    title = "Histogram of Square-Root-Transformed Coral Bleaching (%) — Pacific and Indian Oceans",
    x = "sqrt Percent Bleaching ",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.line = element_line(colour = "black", linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Kolmogorov-Smirnov test for normality
x <- log(bleach$Percent_Bleaching+1)
ks.test(x, "pnorm", mean(x), sd(x))

#Test for difference (non parametric)
wilcox.test(Percent_Bleaching ~ Ocean_Name, data= bleach)

# Test for homogeneity of variance
leveneTest(Percent_Bleaching ~ Ocean_Name, data = bleach)

#Compare median bleaching 
tapply(bleach$Percent_Bleaching, bleach$Ocean_Name, median, na.rm= TRUE)

dev.off()





