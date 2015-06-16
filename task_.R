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

table


h.a <- c(rep(1, 5), rep(2, 5))
h.b <- c("A", "B", "C", "D", "E")
hash.table <- data.frame(h.a, h.b)
hash.table.results <- data.frame(names = paste(hash.table[,1], hash.table[,2], sep="."), value = rep(0, nrow(hash.table)))
hash.table.results <- data.frame(names = 1:10, value = rep(0, nrow(hash.table)))



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

write.table(abc, "abc.csv", row.names=F)

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

hashmaptab(temp[1:5,1, with=F]%>%.[[1]])

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
library(hash) 
#h <- hash( sprintf("%3d", 1:10), hash.table[,2])
#values(h, keys=sprintf("%3d", 1), USE.NAMES=F)
#
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
ht <- new.env(hash = TRUE, size = 10000000L)
temp[1:500000,1, with=F]

keyArray <- paste(hash.table[,1], hash.table[1:5,2], sep=".")
keyArray <- paste(hash.table[1:5,2], sep=".")
for (i in 1:length(keyArray))
{
key <- keyArray[i]
assign(key, i, envir = ht)
}

setnames(temp, paste("V", 1:30, sep=""))

system.time(table(unlist(mget(temp$V1, envir = ht))))
system.time(table(temp[,1,with=F]))

table(unlist(mget(temp$V1, envir = ht)))
table(unlist(mget(temp$V2, envir = ht)))
table(unlist(mget(temp[,1:2, with=F]%>%.[[1]], envir = ht)))





library("fastmatch")
length(fmatch(temp[,1, with=F],"A"))


table

if (exists(word, envir = ht, inherits = FALSE)) {
    oldcount <- get(word, envir = ht)
    assign(word, oldcount + count, envir = ht)
}

fmatch("A", temp[1:10,1, with=F])

x = 
s = "A"
fmatch("A",temp[,1])


keyArray <- ls(envir = testHash)
keyLen <- length(keyArray)

keys <- temp[1,1:10, with=F] %>% as.data.frame %>% .[[1]]
keys
mget(keys, envir = testHash)



apply(temp[1:5,1:10], 2, function(x) mget(x, envir= testHash))


table(temp[1:5, 1:2, with=F])

keys <- sample(ls(envir = testHash), 10000, replace = TRUE)
vals <- mget(keys, envir = testHash)
vals


env <- new.env(hash = TRUE)

library(plyr)
ddply(temp, c("1", "2"), function(x) nrow(x))

library(data.table)
library(stringi)
strings <- rbind("가나다", "다다다", "사사사")
dt <- as.data.table(stri_list2matrix(strsplit(strings, split=""), byrow = TRUE))
unlist(apply(dt, 2, table))


library(data.table)
library(stringr)


a <- readChar("abc.csv",148)
a

mystring <- read_file("path/to/myfile.txt")
abc <- fread("abc.csv")
colnames(abc) <- paste("V", colnames(abc), sep="")
h.b <- c("A", "B", "C", "D", "E")
library(hash)
h <- hash(keys = h.b, values = seq_along(h.b))
values(h, keys = abc$V1[1])

for (i in 1:length(abc$V1))
{
j <- values(h, keys = abc$V1[i])
hash.table.results$value[j] = hash.table.results$value[j] + 1 
}



topology <- Topology(data.frame(abc[1,]))
computeSum <- function(x, ...){
    sum <- GetHash("sum")
    if(is.data.frame(sum)){
        x <- sum + (x[1])
    }
    SetHash("sum", x)
}
topology <- AddBolt(topology, Bolt(computeSum))
result <- RStorm(topology)
print(GetHash("sum", result))
library(RStorm)


a <- readChar("abc.csv",148)
a
colnames(temp) <- paste("V", 1:30, sep="")

hash.table.results[1, mget(temp$V1, envir = ht)]


L <- list(a = 1, b = 2:4, p = pi, ff = gl(3, 4, labels = LETTERS[1:3]))
e <- list2env(L)
size(e)

system.time({
    e <- new.env(parent=emptyenv(), hash=TRUE)
    list2env(ex1, envir=e)
})

library(hash)
.levels <- levels(f)
h <- hash(keys = .levels,values = seq_along(.levels))
newVec <- sample(.levels,10,replace=T)
newVec
values(h,keys = newVec)


e1 <- new.env(parent = baseenv())  # this one has enclosure package:base.
e2 <- new.env(parent = e1)
assign("a", 3, envir = e1)
ls(e1)
ls(e2)
exists("a", envir = e2)   # this succeeds by inheritance
exists("a", envir = e2, inherits = FALSE)
exists("+", envir = e2)   # this succeeds by inheritance

eh <- new.env(hash = TRUE, size = NA)
with(env.profile(eh), stopifnot(size == length(counts)))
ls(eh)

# Set up hash table
keys <- c("John Smith", "Lisa Smith", "Sam Doe", "Sandra Dee", "Ted Baker")
values <- c(152, 1, 254, 152, 153)
names(values) <- keys
# Get value corresponding to a key
values["Sam Doe"]                          # vals["Sam Doe"]
# Get all keys corresponding to a value
names(values)[values==152]                 # "John Smith" "Sandra Dee"

library(stringi)
con <- "abc.csv"
as.character(hash.table$h.b[1:5])

stri_count_fixed(readLines(con, 100), c("A","B","C"))

stri_count_fixed("GCCCAAAATTTTCCGG",c("G","C"))
