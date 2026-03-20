# install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)

# Load data
age_data <- read.csv("figure6b_age_bins.csv", stringsAsFactors = FALSE)
age_data <- age_data[age_data$count > 0, ]

age_data$age_group <- factor(age_data$age_group,
                             levels = c("Under 30", "30-44", "45-59", "60+"))

n_studies_per_year <- c(
  "2012" = 1, "2015" = 3, "2016" = 5, "2017" = 2,
  "2018" = 2, "2020" = 1, "2021" = 1
)

years    <- sort(unique(age_data$year))
x_labels <- paste0(years, "\n(n=", n_studies_per_year[as.character(years)], " studies)")
names(x_labels) <- years

age_data$year_label <- factor(
  x_labels[as.character(age_data$year)],
  levels = x_labels
)

age_data_pos <- age_data %>%
  arrange(year_label, desc(age_group)) %>%
  group_by(year_label) %>%
  mutate(
    cum_top = cumsum(count),
    label_y = cum_top - count / 2   
  ) %>%
  ungroup()

# Plotting
fig6b <- ggplot(age_data_pos,
                aes(x = year_label, y = count, fill = age_group)) +
  
  geom_bar(stat = "identity", position = "stack",
           width = 0.7, color = "white", linewidth = 0.4) +
  
  geom_text(
    aes(y = label_y, label = count),
    color = "white", size = 4.2, fontface = "bold", family = "Helvetica"
  ) +
  
  scale_fill_manual(
    values = c(
      "Under 30" = "#BBDEFB",
      "30-44"    = "#64B5F6",
      "45-59"    = "#1976D2",
      "60+"      = "#0D47A1"
    ),
    name = "Age Group"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.04)),
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  
  labs(
    title    = "Stacked Bar Chart of Age representation of participants in BCI studies by time periods",
    subtitle = "Figure 6b \u2014 Filtered articles by publication year (8 of 20 studies excluded: age not reported and not applicable)",
    x        = "Publication Year",
    y        = "Number of Participants",
    caption  = paste0(
      "Note: Participants with individually reported ages were binned directly.\n",
      "Out of 20 filtered articles, 2 were excluded for being review and protocol papers.\n",
      "Other exclusion (age not reported): Zhang & Koike 2022, Li et al. 2023, Khanam 2022,\n",
      "Vuckovic proof-of-concept 2015, Colamarino 2023 (protocol only), Leite 2017."
    )
  ) +
  
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title         = element_text(size = 14, face = "bold",    hjust = 0),
    plot.subtitle      = element_text(size = 11, color = "grey40", hjust = 0),
    plot.caption       = element_text(size = 8,  color = "grey50", hjust = 0,
                                      lineheight = 1.4),
    axis.title.x       = element_text(size = 11, margin = margin(t = 15)),
    axis.title.y       = element_text(size = 11, margin = margin(r = 15)),
    axis.text          = element_text(size = 10),
    axis.text.x        = element_text(lineheight = 1.3, margin = margin(t = 15)),
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 10),
    legend.position    = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    plot.margin        = margin(t = 15, r = 20, b = 10, l = 15)
  )

print(fig6b)

ggsave("figure6b_age_stacked_bar.png",
       plot = fig6b, width = 14, height = 14, dpi = 300, bg = "white")