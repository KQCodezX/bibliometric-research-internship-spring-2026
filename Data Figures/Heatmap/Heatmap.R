# install.packages(c("ggplot2", "dplyr", "tidyr"))


library(ggplot2)
library(dplyr)
library(tidyr)

# Load data
df <- read.csv("Heatmap_Updated.csv")

sex_data <- df %>% 
  distinct(Article, Sex, BCI_Type) %>%
  count(Characteristic = Sex, BCI_Type) %>% mutate(Category = "Sex")

age_data <- df %>% 
  distinct(Article, Age, BCI_Type) %>%
  count(Characteristic = Age, BCI_Type) %>% mutate(Category = "Age")

injury_data <- df %>%
  distinct(Article, Injury, BCI_Type) %>%
  count(Characteristic = Injury, BCI_Type) %>% mutate(Category = "Injury")

combined_data <- bind_rows(sex_data, age_data, injury_data)

heatmap_data <- combined_data %>%
  complete(BCI_Type, nesting(Characteristic, Category), fill = list(n = 0)) %>%
  mutate(
    Category = factor(Category, levels = c("Sex", "Age", "Injury")),
    Characteristic = factor(Characteristic, levels = rev(c(
      "Male", "Female", "Mixed", "Unspecified",
      "18-35", "36-55", "56+", "Not Reported",
      "Stroke", "SCI", "Amputation", "Healthy"
    )))
  )

# Plot

ggplot(heatmap_data, aes(x = BCI_Type, y = Characteristic, fill = n)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = n), 
            color = "black", 
            size = 4, 
            fontface = "bold", 
            family = "Helvetica") + 
  scale_fill_gradient(low = "#e3f2fd", high = "#1565C0", name = "No. of\nArticles") +
  facet_grid(Category ~ ., scales = "free_y", space = "free_y") + 
  labs(
    title = "Study Population vs. BCI Technology Type",
    subtitle = "Analysis of Top 20 Most Cited Articles",
    x = "BCI Technology Type", y = ""
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    strip.text.y = element_text(angle = 0, face = "bold", size = 11),
    axis.text = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 20), face = "bold"), 
    panel.grid = element_blank(),
    plot.title = element_text(size = 15, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "white"),
    legend.title = element_text(face = "bold", size = 9)
  )

ggsave("BCI_Heatmap_Final_V2.png", width = 14, height = 12, dpi = 1000)