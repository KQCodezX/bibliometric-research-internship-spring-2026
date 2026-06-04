library(bibliometrix)

data <- read.csv("Clean_Keyword_Matrix.csv", row.names = 1)
mat <- as.matrix(data)

png("Keyword_Network_Improved.png", 
    units = "in", 
    width = 12, 
    height = 12, 
    res = 1000)  


# Generate and save the network
Net <- networkPlot(
  mat,                 
  n = 30,
  type = "fruchterman",
  size = TRUE,
  edgesize = 5,          
  labelsize = 0.7,
  Title = "Keyword Co-occurrence Network",
  remove.isolates = TRUE,
)

dev.off()