args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==3)
w <- read.csv(args[1], check.names=FALSE, na.strings=c("", "NA"), fileEncoding="UTF-8-BOM")
s <- read.csv(args[2], check.names=FALSE, na.strings=c("", "NA"), fileEncoding="UTF-8-BOM")
combined <- rbind(data.frame(year=w[["Publication Year"]], doi=w[["DOI Link"]], citations=w[["Times Cited, All Databases"]]),
                  data.frame(year=s[["Year"]], doi=s[["DOI Link"]], citations=s[["Cited by"]]))
combined <- subset(combined, year < 2026)
combined <- combined[!duplicated(combined$doi),]
counts <- as.data.frame(table(combined$year))
names(counts) <- c("year","count")
counts$total <- sapply(counts$year, function(y) sum(combined$citations[combined$year==as.integer(as.character(y))],na.rm=TRUE))
counts$mean <- sapply(counts$year, function(y) mean(combined$citations[combined$year==as.integer(as.character(y))],na.rm=TRUE))
write.csv(counts,args[3],row.names=FALSE)
