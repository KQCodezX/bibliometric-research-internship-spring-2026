pkgs <- c("ggplot2", "dplyr", "tidyr", "scales")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Load Data
df <- read.csv("Figure6DataExtraction.csv", stringsAsFactors = FALSE)

df <- df %>%
  mutate(Period = case_when(
    Year >= 2012 & Year <= 2014 ~ "2012-2014",
    Year >= 2015 & Year <= 2017 ~ "2015-2017",
    Year >= 2018 & Year <= 2020 ~ "2018-2020",
    Year >= 2021 & Year <= 2023 ~ "2021-2023",
    TRUE ~ "Other"
  ))

agg <- df %>%
  group_by(Period) %>%
  summarise(
    Male            = sum(Male,            na.rm = TRUE),
    Female          = sum(Female,          na.rm = TRUE),
    Sex_Unspecified = sum(Sex_Unspecified, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Total = Male + Female + Sex_Unspecified)


agg <- agg %>%
  mutate(
    Male_pct   = round(Male            / Total * 100, 1),
    Female_pct = round(Female          / Total * 100, 1),
    Unspc_pct  = round(Sex_Unspecified / Total * 100, 1)
  )

print(agg %>% select(Period, Total, Male_pct, Female_pct, Unspc_pct))

long <- agg %>%
  select(Period, Total, Male_pct, Female_pct, Unspc_pct) %>%
  pivot_longer(
    cols      = c(Male_pct, Female_pct, Unspc_pct),
    names_to  = "Sex",
    values_to = "Percentage"
  ) %>%
  mutate(
    Sex = recode(Sex,
                 "Male_pct"   = "Male",
                 "Female_pct" = "Female",
                 "Unspc_pct"  = "Unspecified"
    )
  )

STACK_ORDER <- c("Male", "Female", "Unspecified")

period_levels <- c("2012-2014", "2015-2017", "2018-2020", "2021-2023")

plot_data <- long %>%
  mutate(
    Sex    = factor(Sex,    levels = STACK_ORDER),
    Period = factor(Period, levels = period_levels)
  ) %>%
  arrange(Period, Sex) %>%
  group_by(Period) %>%
  mutate(
    ymax    = cumsum(Percentage),
    ymin    = ymax - Percentage,
    label_y = (ymin + ymax) / 2,
    x_pos   = as.numeric(Period)
  ) %>%
  ungroup()

print(plot_data %>% select(Period, Sex, Percentage, ymin, ymax, label_y))

sex_colours <- c(
  "Male"        = "#1565C0",   # deep blue
  "Female"      = "#64B5F6",   # light blue
  "Unspecified" = "#BBDEFB"    # lighest blue
)

BAR_HALF_W <- 0.30

n_labels <- agg %>%
  mutate(
    x_pos   = as.numeric(factor(Period, levels = period_levels)),
    label_y = 103
  )

# Plotting

p <- ggplot() +

  geom_rect(
    data = plot_data,
    aes(
      xmin = x_pos - BAR_HALF_W,
      xmax = x_pos + BAR_HALF_W,
      ymin = ymin,
      ymax = ymax,
      fill = Sex
    ),
    colour    = "white",
    linewidth = 0.9
  ) +

  geom_text(
    data = filter(plot_data, Percentage > 0),
    aes(
      x     = x_pos,
      y     = label_y,
      label = paste0(Percentage, "%"),
      size  = 6
    ),
    fontface = "bold",
    colour   = "white",
    family   = "Helvetica"
  ) +
  scale_size_identity() +

  geom_text(
    data = n_labels,
    aes(x = x_pos, y = label_y, label = paste0("n = ", Total)),
    fontface = "bold",
    size     = 5.5,
    colour   = "#222222",
    family   = "Helvetica"
  ) +

  scale_fill_manual(
    values = sex_colours,
    name   = "Sex / Gender",
    guide  = guide_legend(reverse = FALSE)
  ) +

  scale_x_continuous(
    breaks = 1:4,
    labels = period_levels,
    expand = expansion(add = c(0.55, 0.85))
  ) +

  scale_y_continuous(
    limits = c(0, 108),
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +

  labs(
    title    = "Stacked Bar Chart of the Proportion of Sex/Gender Representation of Participants in BCI Studies by Time Periods",
    subtitle = "Proportion of male, female, and unspecified participants by 3-year time period",
    x        = "Time Period",
    y        = "Proportion of Participants (%)",
    caption  = paste0(
      "Data source: Filtered literature (n = ", nrow(df),
      " studies) out of 20
        Out of 20 filtered articles, 2 were excluded for being review and protocol papers."
    )
  ) +

  theme_minimal(base_family = "Helvetica", base_size = 16) +
  theme(
    plot.title         = element_text(face = "bold", size = 20,
                                      margin = margin(b = 6)),
    plot.subtitle      = element_text(size = 14, colour = "grey45",
                                      margin = margin(b = 20)),
    plot.caption       = element_text(size = 11, colour = "grey55",
                                      margin = margin(t = 16), hjust = 0.5),
    axis.title.x       = element_text(face = "bold", size = 16,
                                      margin = margin(t = 14)),
    axis.title.y       = element_text(face = "bold", size = 16,
                                      margin = margin(r = 14)),
    axis.text          = element_text(size = 15),
    axis.text.x        = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.5),
    legend.position    = "right",
    legend.title       = element_text(face = "bold", size = 14),
    legend.text        = element_text(size = 13),
    legend.key.height  = unit(1.4, "cm"),
    legend.key.width   = unit(1.0, "cm"),
    plot.margin        = margin(25, 30, 25, 25),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA)
  )

print(p)

output_file <- "Figure6_Sex_StackedBarChart.png"
ggsave(
  filename = output_file,
  plot     = p,
  width    = 16,
  height   = 11,
  dpi      = 300,
  bg       = "white"
)


print(
  agg %>%
    select(Period, Total, Male, Female, Sex_Unspecified,
           Male_pct, Female_pct, Unspc_pct) %>%
    rename(
      "N Total"       = Total,
      "N Male"        = Male,
      "N Female"      = Female,
      "N Unspecified" = Sex_Unspecified,
      "% Male"        = Male_pct,
      "% Female"      = Female_pct,
      "% Unspecified" = Unspc_pct
    )
)
