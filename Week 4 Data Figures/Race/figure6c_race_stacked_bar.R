# Remove note to install packages if not installed already
# install.packages(c("ggplot2", "scales"))

library(ggplot2)
library(scales)

# Building data

agg <- data.frame(
  year   = c(2012, 2015, 2015, 2016, 2016, 2017, 2017, 2018, 2018, 2020, 2021, 2022, 2022, 2023, 2023),
  status = c(
    "Not Reported",                          # 2012
    "Not Reported", "Not Reported",          # 2015
    "Not Reported", "Not Reported",          # 2016
    "Not Reported", "Not Reported",          # 2017
    "Not Reported", "Not Reported",          # 2018
    "Not Reported",                          # 2020
    "Not Reported",                          # 2021
    "Not Reported", "Not Reported",          # 2022
    "Not Reported", "Not Reported"           # 2023
  ),
  stringsAsFactors = FALSE
)

extra_2016 <- data.frame(
  year   = c(2016, 2016, 2016),
  status = c("Not Reported", "Not Reported", "Not Reported"),
  stringsAsFactors = FALSE
)
agg <- rbind(agg, extra_2016)

counts <- as.data.frame(table(agg$year, agg$status))
names(counts) <- c("year", "status", "count")
counts <- counts[counts$count > 0, ]
counts$year <- as.integer(as.character(counts$year))

n_per_year  <- as.data.frame(table(agg$year))
names(n_per_year) <- c("year", "n")
n_per_year$year <- as.integer(as.character(n_per_year$year))

counts <- merge(counts, n_per_year, by = "year")

counts$year_label <- factor(
  paste0(counts$year, "\n(n=", counts$n, " studies)"),
  levels = paste0(sort(unique(counts$year)),
                  "\n(n=", n_per_year$n[order(n_per_year$year)], " studies)")
)

counts$status <- factor(counts$status,
                        levels = c("Not Reported"))

cat("\nData check:\n")
print(counts[, c("year","status","count")])
cat("\n")

# Plotting

fig6c <- ggplot(counts, aes(x = year_label, y = count, fill = status)) +
  
  geom_bar(stat = "identity", position = "stack",
           width = 0.65, color = "white", linewidth = 0.4) +
  
  geom_text(
    aes(label = count),
    position = position_stack(vjust = 0.5),
    color = "white", size = 3.5, fontface = "bold", family = "Helvetica"
  ) +
  
  scale_fill_manual(
    values = c(
      "Not Reported" = "#64B5F6"  
    ),
    name = "Race/Ethnicity\nReporting Status"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    breaks = seq(0, 6, by = 1)
  ) +
  
  labs(
    title    = "Stacked Bar Chart of Race Representation of Participants in BCI Studies by Year",
    subtitle = "Figure 6c \u2014 Filtered articles (n=18) by publication year",
    x        = "Publication Year",
    y        = "Number of Studies",
    caption  = paste0(
      "Note: None of the 18 filtered studies out of 20 explicitly reported participant race or ethnicity.\n",
      "Out of 20 filtered articles, 2 were excluded for being review and protocol papers, leaving 18 articles included (n = 18)."
      
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

print(fig6c)

# Save

ggsave("figure6c_race_reporting.png",
       plot = fig6c, width = 11, height = 5.5, dpi = 300, bg = "white")