args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==3)
read_authors <- function(path) read.csv(path, check.names=FALSE, fileEncoding="UTF-8-BOM", colClasses="character")[["Authors"]]
authors <- c(read_authors(args[1]), read_authors(args[2]))
edges <- list()
for (a in authors) {
  if (is.na(a)) next
  names <- trimws(gsub("[[:space:]]+", " ", gsub(",", " ", gsub(".", "", strsplit(a, ";", fixed=TRUE)[[1]], fixed=TRUE))))
  names <- names[names != ""]
  if (length(names)>1) edges[[length(edges)+1]] <- t(combn(names,2))
}
pairs <- as.data.frame(do.call(rbind,edges), stringsAsFactors=FALSE)
names(pairs) <- c("from","to")
g <- igraph::graph_from_data_frame(pairs,directed=FALSE)
igraph::E(g)$weight <- 1
g <- igraph::simplify(g,edge.attr.comb=list(weight="sum","ignore"))
write.csv(igraph::as_data_frame(g,what="edges"),paste0(args[3],"-edges.csv"),row.names=FALSE)
rank <- igraph::page_rank(g,directed=FALSE,damping=.85,weights=igraph::E(g)$weight)$vector
write.csv(data.frame(author=names(rank),rank=unname(rank)),paste0(args[3],"-rank.csv"),row.names=FALSE)
