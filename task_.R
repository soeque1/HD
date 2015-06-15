rm(list=ls())
library(parallel)
table_multi <- function(allFiles){
    mc <- detectCores();
    cl <- makeCluster(mc);
    DT <- parLapply(cl,allFiles,fun=table);
    stopCluster(cl);
    DT <- c(unlist(DT));
    return(DT);
}


h.a <- c(rep(1, 5), rep(2, 5))
h.b <- c("A", "B", "C", "D", "E")
hash.table <- data.frame(h.a, h.b)
hash.table.results <- data.frame(names = paste(hash.table[,1], hash.table[,2], sep="."), value = rep(0, nrow(hash.table)))

a <- c(rep("A", 100000), rep("B", 200000), rep("C", 300000), rep("D", 400000))
b <- c(rep("A", 400000), rep("B", 300000), rep("C", 200000), rep("D", 100000))
a <- rep(a,10)
b <- rep(b,10)
abc <- data.frame(a,b)
abc <- cbind(abc, abc, abc, abc, abc, abc, abc, abc, abc, abc, abc, abc, abc, abc, abc)
#abc <- cbind(abc, abc, abc)

c <- c(rep(2010, length(a)/2), rep(2011, length(a)/2))
abc <- cbind(abc, c)
rm(a); rm(b); rm(c)
colnames(abc) <- 1:ncol(abc)
colnames(abc)[ncol(abc)]<-"year"

library(dplyr)
library(data.table)
years <- 2010:2011
abc <- as.data.table(abc)
setkey(abc, year)
temp <- abc[year == years[1]]
temp <- temp[, !"year", with=F]
#temp <- abc %>% filter(year==years[1]) %>% .[-ncol(abc)] #%>% as.data.table
dim(temp)
#temp[list("A")]

#abc %>% group_by(year) %>% summarise_each(funs(summary(.)))
#table(abc)
#temp %>% summarise_each(funs(length(.)))

system.time(table_multi(temp))
system.time(unlist(lapply(temp, table)))


temp_1 <- data.frame(names = names(temp), value = temp)


library(plyr)
results <- t(merge(hash.table.results, temp_1, by="names", all.x=T)[-2])
results[is.na(results)] <- 0
results


# testHash <- new.env(hash = TRUE, size = 10000L)
# for(i in 1:10000) {
#     key <- paste(sample(letters, 3, replace = TRUE), collapse = "")
#     assign(key, floor(1000*runif(1)), envir = testHash)
# }
# 
# keyArray <- ls(envir = testHash)
# keyLen <- length(keyArray)
# 
# for(j in 1:10000) {
#     key <- keyArray[sample(keyLen, 1)]
#     lookupValue <- get(key, envir = testHash)
#     cat(paste("key", key, "Lookup", lookupValue, "\n"))
# }
# 
# 
# hash.table[,"h.b"]
# testHash <- new.env(hash = TRUE, size = 100L)
# keyArray <- letters[1:10]#$hash.table[,1]
# for (i in keyArray)
# {
# key <- keyArray[i]
# assign(key, hash.table[i,2], envir = testHash)
# }
# 
# keyLen <- length(keyArray)
# 
# for(j in 1:100) {
#     key <- keyArray[j]
#     lookupValue <- get(temp[,1], envir = testHash)
#     A_sum <- A_sum +
# }
# 
# 
# 
# 
# h <- hash( sprintf("%3d", 1:10), hash.table[,2])
# values(h, keys=sprintf("%3d", 1), USE.NAMES=F)
# temp[1:5,1]
# h
# 
# \
# has.key(temp[,1], h)
# temp[1:5,1]
# 
# hasMethod(h, "character", sum)
# 
# all( has.key( letters, h ) ) # TRUE
# 
# 
# library(hash)
# ?has.key
