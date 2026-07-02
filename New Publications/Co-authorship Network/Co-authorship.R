library(igraph)


# Load ur data or csv
scopus_data <- read.csv("Scopus.csv", stringsAsFactors = FALSE)
wos_data    <- read.csv("WoS.csv", stringsAsFactors = FALSE)

# Extract just the first column (Authors) from both datasets to combine them
scopus_authors <- data.frame(Authors = scopus_data[, 1], stringsAsFactors = FALSE)
wos_authors    <- data.frame(Authors = wos_data[, 1], stringsAsFactors = FALSE)

combined_data <- rbind(scopus_authors, wos_authors)

# Cleaning the names and formating
standardize_names <- function(author_str) {
  if (is.na(author_str) || author_str == "") return(NULL)
  
  
  authors <- strsplit(as.character(author_str), ";")[[1]]
  
  
  authors <- sapply(authors, function(name) {
    name <- trimws(name)
    name <- gsub("\\.", "", name)       # Remove periods (Scopus: "Smith J." to "Smith J")
    name <- gsub(",", " ", name)        # Replace commas with spaces (WoS: "Smith, J" to "Smith J")
    name <- gsub("\\s+", " ", name)     # Collapse multiple spaces into a single space
    return(name)
  })
  
  
  authors <- authors[authors != ""]
  return(as.character(authors))
}

# Apply the cleaning function to every row
combined_data$Clean_Authors <- lapply(combined_data$Authors, standardize_names)

# Filter out solo-authored papers
combined_data <- combined_data[sapply(combined_data$Clean_Authors, length) > 1, ]
if (nrow(combined_data) == 0) stop("No co-authored papers found after cleaning. Graph will be empty.")

# Creating weighted edge

edges_list <- lapply(combined_data$Clean_Authors, function(a) {
  if (length(a) > 1) t(combn(a, 2)) else NULL
})

edges_list <- edges_list[!sapply(edges_list, is.null)]
edges <- as.data.frame(do.call(rbind, edges_list), stringsAsFactors = FALSE)
colnames(edges) <- c("from", "to")

# Aggregate duplicate edges into a 'weight' attribute

edges$weight <- 1
edges_weighted <- aggregate(weight ~ from + to, data = edges, sum)

if (nrow(edges_weighted) == 0) stop("No edges available to build the graph.")

# Plotting

g <- graph_from_data_frame(edges_weighted, directed = FALSE)

# Simplify loops but keep the sum of weights
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, 
              edge.attr.comb = list(weight = "sum", "ignore"))

# Community detection and sizing

set.seed(42)
communities    <- cluster_louvain(g)
membership_vec <- membership(communities)

cluster_sizes <- sort(table(membership_vec), decreasing = TRUE)
top_n         <- min(20, length(cluster_sizes)) 
top_clusters  <- as.integer(names(cluster_sizes)[1:top_n])

node_colors <- ifelse(membership_vec %in% top_clusters, "#6ECFF6", "gray75")

node_labels <- ifelse(membership_vec %in% top_clusters, V(g)$name, NA)

# Calculate node size based on degree
deg <- degree(g)
max_deg <- max(deg)
if (max_deg == 0) {
  v_size <- rep(2, vcount(g)) 
} else {
  v_size <- 2 + (deg / max_deg) * 6
}

# Layout and Highlights

set.seed(42)
layout_coords <- layout_with_fr(g)
layout_coords <- norm_coords(layout_coords, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

# Group nodes by cluster to draw background shapes

mark_list <- list()
for (cl in top_clusters) {
  nodes_in_cluster <- which(membership_vec == cl)
  if (length(nodes_in_cluster) >= 3) { 
    mark_list[[as.character(cl)]] <- nodes_in_cluster
  }
}


# Rendering the plot above

png(
  filename = "Integrated_Co-authorship_Network.png",
  width    = 14,
  height   = 14,
  units    = "in",
  res      = 1000,
  bg       = "white"
)

par(mar = c(4, 2, 3, 2))

plot(
  g,
  vertex.size        = v_size,
  vertex.label       = node_labels,
  vertex.label.cex   = 0.45,
  vertex.label.color = "black",
  vertex.label.dist  = 0.5,
  vertex.label.font  = 1,
  vertex.color       = node_colors,
  vertex.frame.color = NA,
  edge.color         = "gray85",
  edge.width         = E(g)$weight * 0.5, 
  layout             = layout_coords,
  rescale            = FALSE,
  xlim               = c(-1, 1),
  ylim               = c(-1, 1),
  margin             = 0,
  mark.groups        = mark_list, 
  mark.col           = adjustcolor("#1B4F8A", alpha.f = 0.1),
  mark.border        = adjustcolor("#1B4F8A", alpha.f = 0.4)
)

legend(
  "topright",
  legend = c("Top Cluster Author", "Other"),
  pt.bg  = c("#6ECFF6", "gray75"),
  pch    = 21,
  pt.cex = 2,
  bty    = "n",
  title  = "Author Type",
  cex    = 1.0
)

title(
  main     = "Top 20 Co-authorship Network Research Clusters",
  cex.main = 1.3
)

mtext(
  "Figure 7. Co-authorship Network",
  side = 1,
  line = 1,
  cex  = 1.0,
  font = 3,    
  adj  = 0.5
)

dev.off()