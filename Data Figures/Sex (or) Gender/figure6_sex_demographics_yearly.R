# install.packages(c("ggplot2", "dplyr", "tidyr", "scales"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Loading Data From .csv
data <- read.csv("Figure6DataExtraction.csv", stringsAsFactors = FALSE)
names(data) <- c("article", "year", "male", "female", "unspecified")

# Aggregate by year
years_with_data <- sort(unique(data$year))
year_chars      <- as.character(years_with_data)

male_sum  <- tapply(data$male,          data$year, sum)[year_chars]
fem_sum   <- tapply(data$female,        data$year, sum)[year_chars]
unsp_sum  <- tapply(data$unspecified,   data$year, sum)[year_chars]
n_studies <- tapply(rep(1, nrow(data)), data$year, sum)[year_chars]

for (y in year_chars) {
  cat(y, "| Studies:", n_studies[y],
      "| Male:", male_sum[y], "| Female:", fem_sum[y],
      "| Unspecified:", unsp_sum[y],
      "| Total:", male_sum[y] + fem_sum[y] + unsp_sum[y], "\n")
}

n_years <- length(years_with_data)

agg <- data.frame(
  year  = rep(years_with_data, 3),
  Sex   = c(rep("Male", n_years), rep("Female", n_years), rep("Unspecified", n_years)),
  Count = c(male_sum[year_chars], fem_sum[year_chars], unsp_sum[year_chars]),
  stringsAsFactors = FALSE
)

x_labels        <- paste0(years_with_data, "\n(n=", n_studies[year_chars], ")")
names(x_labels) <- years_with_data

agg$year_label <- factor(x_labels[as.character(agg$year)], levels = x_labels)
agg$Sex        <- factor(agg$Sex, levels = c("Male", "Female", "Unspecified"))

agg <- agg[agg$Count > 0, ]

agg_pos <- agg %>%
  arrange(year_label, desc(Sex)) %>%
  group_by(year_label) %>%
  mutate(
    cum_top = cumsum(Count),
    label_y = cum_top - Count / 2
  ) %>%
  ungroup()

# Plotting
fig6 <- ggplot(agg_pos, aes(x = year_label, y = Count, fill = Sex)) +
  
  geom_bar(stat = "identity", position = "stack",
           width = 0.65, color = "white", linewidth = 0.4) +
  
  geom_text(
    aes(y = label_y, label = Count),
    color = "white", size = 4.2, fontface = "bold", family = "Helvetica"
  ) +
  
  scale_fill_manual(
    values = c("Male" = "#1565C0", "Female" = "#64B5F6", "Unspecified" = "#BBDEFB"),
    name   = "Sex / Gender"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    limits = c(0, 80),
    breaks = seq(0, 80, by = 10)
  ) +
  
  labs(
    title    = "Stacked Bar Chart of Sex/Gender Representation of Participants in BCI Studies by Year",
    subtitle = "Filtered articles (n=18) by publication year",
    x        = "Publication Year",
    y        = "Number of Participants",
    caption  = paste0(
      "Note: 'Unspecified' = sex/gender not reported in the article.\n",
      "n = number of studies published that year.\n",
      "Out of 20 filtered articles, 2 were excluded for being review and protocol papers, ",
      "leaving 18 articles included (n = 18)."
    )
  ) +
  
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title         = element_text(size = 14, face = "bold",    hjust = 0, margin = margin(b = 6)),
    plot.subtitle      = element_text(size = 11, color = "grey40", hjust = 0, margin = margin(b = 20)),
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
    plot.margin        = margin(t = 30, r = 20, b = 10, l = 15)
  )

print(fig6)

ggsave("figure6_sex_demographics.png",
       plot = fig6, width = 14, height = 13, dpi = 300, bg = "white")