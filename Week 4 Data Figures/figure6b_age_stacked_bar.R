
# Remove note to install packages if not installed already
# install.packages(c("ggplot2", "scales"))

library(ggplot2)
library(scales)


# Loading data
# Make sure figure6b_age_bins.csv is in your working directory (in same folder as R script)


age_data <- read.csv("figure6b_age_bins.csv", stringsAsFactors = FALSE)

age_data <- age_data[age_data$count > 0, ]

age_data$age_group <- factor(age_data$age_group,
  levels = c("Under 30", "30-44", "45-59", "60+"))

n_studies_per_year <- c(
  "2015" = 3, "2016" = 5, "2017" = 2,
  "2018" = 2, "2020" = 1, "2021" = 1
)

years <- sort(unique(age_data$year))
x_labels <- paste0(years, "\n(n=", n_studies_per_year[as.character(years)], " studies)")
names(x_labels) <- years

age_data$year_label <- factor(
  x_labels[as.character(age_data$year)],
  levels = x_labels
)

age_data$display_count <- ifelse(age_data$count > 0 & age_data$count < 4, 4, age_data$count)

# Plotting

fig6b <- ggplot(age_data, aes(x = year_label, y = display_count, fill = age_group)) +

  geom_bar(stat = "identity", position = "stack",
           width = 0.65, color = "white", linewidth = 0.4) +

  geom_text(
    data     = age_data[age_data$count > 0, ],
    aes(label = count),
    position = position_stack(vjust = 0.5),
    color    = "white", size = 3.5, fontface = "bold", family = "Helvetica"
  ) +

  scale_fill_manual(
    values = c(
      "Under 30" = "#BBDEFB",   # lightest blue
      "30-44"    = "#64B5F6",   # light blue
      "45-59"    = "#1976D2",   # medium blue
      "60+"      = "#0D47A1"    # darkest blue
    ),
    name = "Age Group"
  ) +

  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    breaks = pretty_breaks(n = 6)
  ) +

  labs(
    title    = "Age Representation of Participants in BCI-Spinal Cord Injury Studies",
    subtitle = "Figure 6b — Filtered articles by publication year (6 of 17 studies excluded: age not reported)",
    x        = "Publication Year",
    y        = "Number of Participants",
    caption  = paste0(
      "Note: Participants with individually reported ages were binned directly.\n",
      "Participants from studies reporting only mean age were assigned to the bin containing their group mean.\n",
      "Excluded (age not reported): Zhang & Koike 2022, Li et al. 2023, Khanam 2022,\n",
      "Vuckovic proof-of-concept 2015, Colamarino 2023 (protocol only), Leite 2017.\n",
      "Small bars padded for readability; labels show true counts."
    )
  ) +

  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title         = element_text(size = 13, face = "bold", hjust = 0),
    plot.subtitle      = element_text(size = 10, color = "grey40", hjust = 0),
    plot.caption       = element_text(size = 7.5, color = "grey50", hjust = 0,
                                      lineheight = 1.4),
    axis.title.x       = element_text(size = 10, margin = margin(t = 15)),
    axis.title.y       = element_text(size = 10, margin = margin(r = 15)),
    axis.text          = element_text(size = 8.5),
    axis.text.x        = element_text(lineheight = 1.3, margin = margin(t = 15)),
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 9),
    legend.position    = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    plot.margin        = margin(t = 15, r = 15, b = 10, l = 15)
  )

print(fig6b)

# Save

ggsave("figure6b_age_stacked_bar.png",
       plot = fig6b, width = 11, height = 5.5, dpi = 300, bg = "white")
