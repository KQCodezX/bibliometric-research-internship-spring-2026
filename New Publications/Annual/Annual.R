required_pkgs <- c("tidyverse", "patchwork", "scales", "ggtext", "showtext")
to_install   <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(tidyverse)
library(patchwork)
library(scales)
library(ggtext)
library(showtext)

font_add_google("Roboto", "helvetica")

# Loading data

scopus_raw <- read_csv("Scopus.csv", show_col_types = FALSE)
wos_raw    <- read_csv("WoS.csv",    show_col_types = FALSE)

# Scopus: standardise column names
scopus <- scopus_raw %>%
  transmute(
    year      = as.integer(Year),
    citations = as.numeric(`Cited by`),
    doi       = `DOI Link`,
    source    = "Scopus"
  )

# WoS: standardise column names
wos <- wos_raw %>%
  transmute(
    year      = as.integer(`Publication Year`),
    citations = as.numeric(`Times Cited, All Databases`),
    doi       = `DOI Link`,
    source    = "Web of Science"
  )

# Merge and deduplicate on DOI (prefer WoS record when duplicate)
combined <- bind_rows(wos, scopus) %>%
  # Remove 2026 (incomplete year — biases growth rate)
  filter(year < 2026) %>%
  # Deduplicate: keep first occurrence (WoS prioritised by bind order)
  group_by(doi) %>%
  slice(1) %>%
  ungroup()


annual <- combined %>%
  group_by(year) %>%
  summarise(
    n_pubs      = n(),
    total_cites = sum(citations, na.rm = TRUE),
    mean_cites  = mean(citations, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(year) %>%
    mutate(
      cumulative_pubs = cumsum(n_pubs),
      growth_rate = (n_pubs / lag(n_pubs) - 1) * 100
    )

# Overall CAGR (first to last full year)
first_yr  <- filter(annual, year == min(year))$n_pubs
last_yr   <- filter(annual, year == max(year))$n_pubs
n_yrs     <- max(annual$year) - min(annual$year)
cagr_pct  <- ((last_yr / first_yr)^(1 / n_yrs) - 1) * 100
cat(sprintf("Overall CAGR (%d–%d): %.1f%%\n", min(annual$year), max(annual$year), cagr_pct))

palette <- list(
  bar       = "#1565C0",     # deep blue — publications
  line      = "grey20",      # red — growth rate
  cites     = "#1565C0",      # teal — citations
  mean_cite = "#1565C0",      # burnt orange — mean citations
  cumul     = "#1565C0",      # purple — cumulative
  grid      = "grey90",
  text      = "black"
)

base_theme <- theme_minimal(base_family = "helvetica", base_size = 11) +
  theme(
    plot.background      = element_rect(fill = "white", colour = NA),
    panel.background     = element_rect(fill = "white", colour = NA),
    panel.grid.major.y   = element_line(colour = palette$grid, linewidth = 0.4),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    axis.text            = element_text(colour = palette$text, size = 9),
    axis.title           = element_text(colour = palette$text, size = 9.5,
                                        face = "bold"),
    axis.ticks           = element_line(colour = "grey70"),
    plot.title           = element_textbox_simple(
      family = "helvetica", size = 10.5, face = "bold",
      colour = palette$text, margin = margin(b = 4)),
    plot.subtitle        = element_text(size = 8.5, colour = "grey45",
                                        margin = margin(b = 6)),
    legend.position      = "none",
    plot.margin          = margin(8, 12, 6, 8)
  )


scale_factor <- max(annual$n_pubs, na.rm = TRUE) /
  max(abs(annual$growth_rate), na.rm = TRUE) * 0.55

p_growth <- ggplot(annual, aes(x = year)) +
  # Bar annual publication count
  geom_col(aes(y = n_pubs), fill = palette$bar, alpha = 0.85, width = 0.65,
           colour = "white", linewidth = 0.25) +
  # Count labels above bars
  geom_text(aes(y = n_pubs, label = n_pubs),
            vjust = -0.45, size = 2.8, colour = palette$bar,
            fontface = "bold", family = "helvetica") +
  
  # CAGR annotation
  annotate("richtext", x = min(annual$year) + 0.5,
           y = max(annual$n_pubs) * 0.96,
           label = sprintf("<b>CAGR = %.1f%%</b><br><span style='font-size:7pt'>(%d–%d)</span>",
                           cagr_pct, min(annual$year), max(annual$year)),
           hjust = 0, size = 3.1, fill = NA, label.color = NA,
           colour = palette$bar, family = "helvetica") +
  scale_x_continuous(breaks = seq(min(annual$year), max(annual$year), 2),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(
    name     = "Annual publications (n)",
    limits   = c(0, max(annual$n_pubs) * 1.18),
    expand   = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(~ . / scale_factor,
                        name   = "Year-on-year growth rate (%)",
                        labels = label_percent(scale = 1, accuracy = 1))
  ) +
  # Right-axis colour
  theme(axis.title.y.right = element_text(colour = palette$line),
        axis.text.y.right  = element_text(colour = palette$line)) +
  labs(
    title    = "A. Annual publication output and growth rate",
    subtitle = "Bars = publications per year (left axis) · Line = YoY growth rate (right axis)",
    x        = NULL
  ) +
  base_theme +
  theme(axis.title.y.right = element_text(colour = palette$line,
                                          face = "bold", size = 9.5),
        axis.text.y.right  = element_text(colour = palette$line, size = 9))

# Panel B
p_cumul <- ggplot(annual, aes(x = year, y = cumulative_pubs)) +
  geom_area(fill = palette$cumul, alpha = 0.15) +
  geom_line(colour = palette$cumul, linewidth = 1.1) +
  geom_point(colour = palette$cumul, size = 2.2) +
  geom_text(aes(label = cumulative_pubs),
            vjust = -0.55, size = 2.7, colour = palette$cumul,
            fontface = "bold", family = "helvetica") +
  scale_x_continuous(breaks = seq(min(annual$year), max(annual$year), 2),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15)),
                     breaks = pretty_breaks(5)) +
  labs(
    title    = "B. Cumulative publication growth",
    subtitle = "Total unique records (deduplicated across Scopus and Web of Science)",
    x        = NULL,
    y        = "Cumulative publications (n)"
  ) +
  base_theme

# Panel C

p_total_cites <- ggplot(annual, aes(x = year, y = total_cites)) +
  geom_col(fill = palette$cites, alpha = 0.82, width = 0.65,
           colour = "white", linewidth = 0.25) +
  geom_text(aes(label = comma(total_cites)),
            vjust = -0.45, size = 2.6, colour = palette$cites,
            fontface = "bold", family = "helvetica") +
  scale_x_continuous(breaks = seq(min(annual$year), max(annual$year), 2),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15)),
                     breaks = pretty_breaks(5)) +
  labs(
    title    = "C. Total citations per year",
    subtitle = "Sum of all citations received by publications in each year",
    x        = NULL,
    y        = "Total citations (n)"
  ) +
  base_theme

# Panel D
p_mean_cites <- ggplot(annual, aes(x = year, y = mean_cites)) +
  geom_line(colour = palette$mean_cite, linewidth = 1.0) +
  geom_point(colour = palette$mean_cite, size = 2.3) +
  geom_area(fill = palette$mean_cite, alpha = 0.12) +
  geom_text(aes(label = round(mean_cites, 1)),
            vjust = -0.55, size = 2.6, colour = palette$mean_cite,
            fontface = "bold", family = "helvetica") +
  # Trend annotation
  annotate("text", x = max(annual$year) - 0.4,
           y = max(annual$mean_cites) * 0.98,
           label = "Recent articles\nhave lower citations\n(citation lag effect)",
           hjust = 1, size = 2.5, colour = "grey50",
           lineheight = 1.2, family = "helvetica") +
  scale_x_continuous(breaks = seq(min(annual$year), max(annual$year), 2),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.18)),
                     breaks = pretty_breaks(5)) +
  labs(
    title    = "D. Mean citations per publication",
    subtitle = "Average citations per paper published in each year (note: citation lag for recent years)",
    x        = "Publication year",
    y        = "Mean citations per paper"
  ) +
  base_theme

# Export

figure <- (p_growth | p_cumul) / (p_total_cites | p_mean_cites) +
  plot_annotation(
    title   = "Annual Growth Rate of BCI Literature: Publication and Citation Trends",
    subtitle = sprintf(
      "Combined Scopus (n = %d) and Web of Science (n = %d) records, %d to %d · Total unique records: %d · CAGR: %.1f%%",
      nrow(scopus_raw), nrow(wos_raw),
      min(annual$year), max(annual$year), nrow(combined), cagr_pct
    ),
    caption = "Figure 1: Annual Growth Rate of BCI Literature: Publication and Citation Trends.\nSources: Scopus and Web of Science. Duplicate records identified by DOI and removed (WoS record retained).\nCAGR = Compound Annual Growth Rate. YoY = Year-on-Year. Citation lag affects recent-year mean citation values.\nAnalysis restricted to 2010-2025 (2026 excluded as incomplete year).",
    theme = theme(
      plot.title    = element_text(family = "helvetica", size = 14,
                                   face = "bold", colour = "grey15",
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(family = "helvetica", size = 9,
                                   colour = "grey40", margin = margin(b = 6)),
      
      plot.caption  = element_text(family = "helvetica", size = 7.5,
                                   colour = "grey55", lineheight = 1.4,
                                   margin = margin(t = 8), 
                                   hjust = 0.5), 
      
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

ggsave("Fig1_BCI_Publication_Citation_Trends.png",
       plot   = figure,
       width  = 16, height = 12, units = "in",
       dpi    = 1000)

annual %>%
  select(year, n_pubs, cumulative_pubs, growth_rate, total_cites, mean_cites) %>%
  mutate(growth_rate = round(growth_rate, 1),
         mean_cites  = round(mean_cites, 1)) %>%
  print(n = Inf)