if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, igraph, ggraph)


# Insert data

scopus_raw <- read.csv("Scopus.csv", stringsAsFactors = FALSE)
wos_raw    <- read.csv("WoS.csv", stringsAsFactors = FALSE)

# Standardize Scopus 
df_scopus <- scopus_raw %>%
  select(Year, auth = Author.Keywords, sec = Index.Keywords) %>%
  mutate(doc_id = paste0("S_", row_number()))

# Standardize WoS 
df_wos <- wos_raw %>%
  select(Year = Publication.Year, auth = Author.Keywords, sec = Keywords.Plus) %>%
  mutate(doc_id = paste0("W_", row_number()))

merged_corpus <- bind_rows(df_scopus, df_wos) %>%
  mutate(
    target_kws = coalesce(na_if(str_trim(auth), ""), na_if(str_trim(sec), ""))
  ) %>%
  filter(!is.na(target_kws), !is.na(Year))


# Data cleaning and keyword mapping

clean_and_harmonize <- function(text) {
  kws <- unlist(strsplit(text, ";")) %>% str_trim() %>% str_to_lower()
  
  mapped <- case_when(
    # --- DOMAIN SPECIFIC TERMS ---
    str_detect(kws, "brain.computer|brain.machine|bci|bmi|neural.interface") ~ "BCI",
    str_detect(kws, "electroencephalograph|^eeg$") ~ "EEG",
    str_detect(kws, "functional.magnetic.resonance|fmri") ~ "fMRI",
    str_detect(kws, "near.infrared|fnirs") ~ "fNIRS",
    str_detect(kws, "^mi$|motor.imagery") ~ "Motor Imagery",
    str_detect(kws, "spinal.cord.injury|^sci$") ~ "Spinal Cord Injury",
    str_detect(kws, "phantom.limb") ~ "Phantom Limb Pain",
    str_detect(kws, "electrocorticograph|ecog") ~ "ECoG",
    str_detect(kws, "neuropathic.pain") ~ "Neuropathic Pain",
    str_detect(kws, "virtual.reality|^vr$") ~ "Virtual Reality",
    str_detect(kws, "deep.learning|^dl$") ~ "Deep Learning",
    str_detect(kws, "machine.learning|^ml$") ~ "Machine Learning",
    str_detect(kws, "transcranial.magnetic") ~ "TMS",
    str_detect(kws, "^human")   ~ "Human",    
    str_detect(kws, "^adult")   ~ "Adult",    
    str_detect(kws, "^patient") ~ "Patient",  
    str_detect(kws, "^male")    ~ "Male",     
    str_detect(kws, "^female")  ~ "Female",
    
    TRUE ~ str_to_title(kws)
  )
  return(mapped)
}


df_long <- merged_corpus %>%
  mutate(clean_kw = map(target_kws, clean_and_harmonize)) %>%
  unnest(clean_kw) %>%
  filter(clean_kw != "") %>%

  distinct(doc_id, Year, clean_kw)


# CALCULATE TEMPORAL METADATA & MATRIX

# Filter to top N keywords

TOP_N <- 30

kw_stats <- df_long %>%
  group_by(clean_kw) %>%
  summarise(
    freq = n(),
    mean_pub_year = mean(Year, na.rm = TRUE)
  ) %>%
  arrange(desc(freq)) %>%
  slice_head(n = TOP_N)

# Filter corpus to just the top concepts
df_top <- df_long %>% filter(clean_kw %in% kw_stats$clean_kw)

# Build Incidence Matrix
inc_matrix <- table(df_top$doc_id, df_top$clean_kw)
co_matrix  <- t(inc_matrix) %*% inc_matrix


diag(co_matrix) <- 0


# BUILD GRAPH OBJECT & PLOT 

g <- graph_from_adjacency_matrix(co_matrix, mode = "undirected", weighted = TRUE)

V(g)$freq      <- kw_stats$freq[match(V(g)$name, kw_stats$clean_kw)]
V(g)$mean_year <- kw_stats$mean_pub_year[match(V(g)$name, kw_stats$clean_kw)]

g_pruned <- delete_edges(g, E(g)[weight < 2])
g_pruned <- delete_vertices(g_pruned, degree(g_pruned) == 0)

# PLOT
set.seed(123) # Locks layout in place so you can recreate it

temporal_plot <- ggraph(g_pruned, layout = "fr") +
  # Draw co-occurrence ties
  geom_edge_link(aes(width = weight), alpha = 0.25, color = "#888888") +
  
  # Draw Nodes: Size = Frequency | Color = Historical Recency
  geom_node_point(aes(size = freq, color = mean_year)) +
  
  # Draw Labels
  geom_node_text(aes(label = name), repel = TRUE, size = 3.8, fontface = "bold") +
  
  # Scales & Visual 
  scale_edge_width(range = c(0.4, 2.5), guide = "none") +
  scale_size(range = c(4, 14), name = "Total Occurrences") +
  scale_color_gradient(
    low = "#1E3A8A",
    high = "#38BDF8",
    name = "Mean Pub. Year"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.05),
    plot.subtitle = element_text(size = 11, hjust = 0.05, color = "#444444"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    title = "Conceptual Evolution of BCI Literature",
    subtitle = "Spatial proximity indicates thematic co-occurrence; node color indicates historical recency."
  )+
  labs(
    caption = "Figure 6. Keyword Co-occurrence Network"
  )

ggsave("Figure_6_Keyword_Evolution.png", plot = temporal_plot, width = 10, height = 7, bg="white", dpi = 1000)