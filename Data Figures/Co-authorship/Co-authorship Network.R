library(igraph)

data <- read.csv("Dataextraction.csv", stringsAsFactors = FALSE)
colnames(data)[1] <- "Authors"

data$Authors <- lapply(strsplit(as.character(data$Authors), ";"), trimws)
data <- data[sapply(data$Authors, length) > 1, ]

edges_list <- lapply(data$Authors, function(a) {
  if (length(a) > 1) t(combn(a, 2)) else NULL
})
edges_list <- edges_list[!sapply(edges_list, is.null)]
edges <- as.data.frame(do.call(rbind, edges_list), stringsAsFactors = FALSE)
colnames(edges) <- c("from", "to")

g <- graph_from_data_frame(edges, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

set.seed(42)
communities  <- cluster_louvain(g)
membership_vec <- membership(communities)

cluster_sizes <- sort(table(membership_vec), decreasing = TRUE)
top_n         <- 20
top_clusters  <- as.integer(names(cluster_sizes)[1:top_n])

node_colors <- ifelse(membership_vec %in% top_clusters, "#6ECFF6", "gray75")

labels      <- sub(",.*", "", V(g)$name)
node_labels <- ifelse(membership_vec %in% top_clusters, labels, NA)

deg    <- degree(g)
v_size <- 2 + (deg / max(deg)) * 6

set.seed(42)
layout_coords <- layout_with_fr(g)
layout_coords <- norm_coords(layout_coords, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

png(
  filename = "Co-authorship_Network_clusters.png",
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
  edge.width         = 0.5,
  layout             = layout_coords,
  rescale            = FALSE,
  xlim               = c(-1, 1),
  ylim               = c(-1, 1),
  margin             = 0
)

# For the box

for (cl in top_clusters) {
  node_idx <- which(membership_vec == cl)
  if (length(node_idx) < 2) next
  
  x_vals  <- layout_coords[node_idx, 1]
  y_vals  <- layout_coords[node_idx, 2]
  padding <- 0.05
  
  rect(
    xleft   = min(x_vals) - padding,
    ybottom = min(y_vals) - padding,
    xright  = max(x_vals) + padding,
    ytop    = max(y_vals) + padding,
    border  = "#1B4F8A",
    lwd     = 2.5,
    col     = NA
  )
}

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
  main     = "Top 20 Co-authorship Netowrk Research Cluster",
  cex.main = 1.3
)

mtext(
  "Figure 12. Co-authorship Network.",
  side = 1,
  line = 1,
  cex  = 1.0,
  font = 3,    
  adj  = 0.5
)

dev.off()
