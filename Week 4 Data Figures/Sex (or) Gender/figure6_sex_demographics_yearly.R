# Remove note to install packages if not done before
#install.packages(c("ggplot2", "dplyr", "tidyr", "scales")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


# Data withou csv (Remove #Note to use)
#data <- data.frame(
#  article = c(
#   "Rehabilitation of hand in subacute tetraplegic patients",
#    "Long-Term Training with BMI-Based Gait Protocol",
#   "Decoding of Pain Perception - Case Study",
#    "Motor imagery in SCI modulated by somatotopic coding",
#    "Influence of central neuropathic pain on BCI performance",
#   "Optimized AI technique for identifying motor imagery from EEGs",
#    "Effects of BCI-controlled FES on Shoulder Subluxation",
#    "DiSCIoser RCT",
#    "BCI targeting non-motor functions - case report",
#    "Clustered ERSP feature in right hand motor imagery",
#    "Coherence based GCN for motor imagery after SCI",
#    "Preservation of hand movement representation in amputees",
#    "Surface EEG-tDCS Closed-Loop System",
#    "Imagine squeezing a cactus - affective motor imagery",
#    "Effects of Peripheral Haptic Feedback on BCI Control",
#   "Hybrid BCI and FES for Tetraplegia",
#    "Training with BMI visuotactile feedback and locomotion"
#  ),
#  year        = c(2016,2016,2020,2017,2015,2022,2016,2023,2015,2022,2023,2017,2017,2018,2021,2015,2018,2012),
#  male        = c(  12,   6,   0,  42,  20,   0,  10,   0,   1,   7,   0,  12,   3,  10,   1,   2,   6, 2),
#  female      = c(   0,   2,   0,   7,   8,   0,  10,   0,   0,   2,   0,   5,   3,  10,   0,   0,   2, 0),
#  unspecified = c(   0,   0,   1,   0,   0,   5,   0,  30,   0,   0,  25,   0,   0,   0,   0,   0,   0, 0),
#  stringsAsFactors = FALSE
#)

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
      "| Total:", male_sum[y]+fem_sum[y]+unsp_sum[y], "\n")
}




n_years <- length(years_with_data)

agg <- data.frame(
  year  = rep(years_with_data, 3),
  Sex   = c(rep("Male",n_years), rep("Female",n_years), rep("Unspecified",n_years)),
  Count = c(male_sum[year_chars], fem_sum[year_chars], unsp_sum[year_chars]),
  stringsAsFactors = FALSE
)

x_labels        <- paste0(years_with_data, "\n(n=", n_studies[year_chars], ")")
names(x_labels) <- years_with_data

agg$year_label <- factor(x_labels[as.character(agg$year)], levels = x_labels)
agg$Sex        <- factor(agg$Sex, levels = c("Male","Female","Unspecified"))

agg$display_count <- ifelse(agg$Count > 0 & agg$Count < 4, 4, agg$Count)

# Plotting

fig6 <- ggplot(agg, aes(x = year_label, y = display_count, fill = Sex)) +
  
  geom_bar(stat = "identity", position = "stack",
           width = 0.65, color = "white", linewidth = 0.4) +
  
  geom_text(
    data     = agg[agg$Count > 0, ],
    aes(label = Count),
    position = position_stack(vjust = 0.5),
    color = "white", size = 3.5, fontface = "bold", family = "Helvetica"
  ) +
  
  scale_fill_manual(
    values = c("Male"="#1565C0","Female"="#64B5F6","Unspecified"="#BBDEFB"),
    name   = "Sex / Gender"
  ) +
  
  scale_y_continuous(expand = expansion(mult=c(0,0.08)), breaks = pretty_breaks(n=6)) +
  
  labs(
    title    = "Stacked Bar Chart of Sex/Gender representation of participants in BCI studies by time periods",
    subtitle = "Filtered articles (n=18) by publication year",
    x        = "Publication Year",
    y        = "Number of Participants",
    caption  = "Note: 'Unspecified' = sex/gender not reported in the article.\nn = number of studies published that year.\nOut of 20 filtered articles, 2 were excluded for being review and protocol papers, leaving 18 articles included (n = 18)."
  ) +
  
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title         = element_text(size=13, face="bold", hjust=0),
    plot.subtitle      = element_text(size=10, color="grey40", hjust=0),
    plot.caption       = element_text(size=8,  color="grey50", hjust=0),
    axis.title.x = element_text(size = 10, margin = margin(t = 15)),
    axis.title.y = element_text(size = 10, margin = margin(r = 15)),
    axis.text.x  = element_text(size = 8.5, lineheight = 1.3, margin = margin(t = 15)),
    axis.text          = element_text(size=8.5),
    legend.title       = element_text(size=9, face="bold"),
    legend.text        = element_text(size=9),
    legend.position    = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color="grey88", linewidth=0.4),
    plot.margin        = margin(t=15, r=15, b=10, l=15)
  )

print(fig6)

ggsave("figure6_sex_demographics.png",
       plot=fig6, width=12, height=7, dpi=300, bg="white")