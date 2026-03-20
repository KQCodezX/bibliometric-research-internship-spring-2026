library(igraph)

authors <- read.csv("Dataextraction.csv")

data <- authors
colnames(data)[1] <- "Authors"

data$Authors <- strsplit(as.character(data$Authors), ";")
data$Authors <- lapply(data$Authors, trimws)

data <- data[sapply(data$Authors, length) > 1, ]

edges_list <- lapply(data$Authors, function(a) {
  if(length(a) > 1) t(combn(a, 2)) else NULL
})

edges_list <- edges_list[!sapply(edges_list, is.null)]

edges <- do.call(rbind, edges_list)

edges <- as.data.frame(edges)
colnames(edges) <- c("from", "to")

g <- graph_from_data_frame(edges, directed = FALSE)

plot(g,
     vertex.size = 3,
     vertex.label = NA,
     vertex.color = "steelblue",
     edge.color = "gray",
     layout = layout_with_fr)