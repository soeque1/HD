dweibull3 <- function(x, location,scale,shape) {shape/scale*((x -location)/scale)^(shape-1)*exp(-((x-location)/scale)^shape)}
pweibull3 <- function(q, location,scale,shape) {1 - exp(-((q-location)/scale)^shape)}

set.seed(123)
library("FAdist")
x3 <- rweibull3(1000, thres = 5, scale = 100, shape = 3)
hist(x3)
#x3 <- rweibull(100, shape = 4, scale = 100)                             #
library(fitdistrplus)

w3den <- function(x, a,b,c) {c/b*((x -a)/b)^(c-1)*exp(-((x-a)/b)^c)}
a <- MASS::fitdistr(x3, w3den, start= list(a = 0, b = mean(x3), c = 2))      #
b <- fitdist(x3, "weibull3", start= list(location = 0, scale = mean(x3), shape = 2))
b
gofstat(b)


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



a <- c(rep("A", 1), rep("B", 2), rep("C", 3), rep("D", 4))
b <- c(rep("A", 4), rep("B", 3), rep("C", 2), rep("D", 1))
c <- c(rep("D", 4), rep("C", 3), rep("B", 2), rep("A", 1))
a <- paste(a, collapse = "")
b <- paste(b, collapse = "")
c <- paste(c, collapse = "")
abc <- rbind(a, b, c)
for (i in 1:2)
{
abc <- rbind(abc, abc)
}

library(data.table)
abc <- as.data.table(abc)
dim(abc)

b <- paste(b, collapse = "")
ab <- paste(a, b, sep = "")
ba <- paste(b, a, sep = "")
abc <- data.frame(ab)
abc <- rbind(abc, ba)

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
#values(h, keys = abc$V1[1])

testHash <- new.env(hash = TRUE, size = 10000L)
for(i in 1:5) {
    key <- h.b[i]
    assign(key, i, envir = testHash)
}




mget(abc[[1]], testHash)



library(dplyr)





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

################

h.a <- c(rep(1, 5), rep(2, 5))
h.b <- c("A", "B", "C", "D", "E")
hash.table <- data.frame(h.a, h.b)
hash.table.results <- data.frame(names = paste(hash.table[,1], hash.table[,2], sep="."), value = rep(0, nrow(hash.table)))
hash.table.results <- data.frame(names = 1:10, value = rep(0, nrow(hash.table)))

testHash1 <- new.env(hash = TRUE, size = 10000L)
for(i in 1:5) {
 key <- hash.table[i,2]
 assign(key, i, envir = testHash1)
}

testHash2 <- new.env(hash = TRUE, size = 10000L)
for(i in 1:5) {
    key <- hash.table[i,2]
    assign(key, i, envir = testHash2)
}

testHash <- list(testHash1, testHash2)

let <- c("A", "B", "C", "D", "E")

characters <- NULL
for(i in 1:1000000) {
    characters <- rbind(characters, paste(let[as.integer( runif(200,1,5.99))], collapse="")) 
}

sourceCpp("table.cpp")
tableC(unlist(mget(substr(characters,1,1), envir = testHash[[1]])))


#get("A", testHash)





unlist(mget(make.keys(A), testHash))



memory <- new.env(hash = TRUE, size = 10000L)
P_12<-function(n,k) {
    if (k<0||k>n) return(0)
    if (n==0 && k==0) return(1)
    if (k==1) return(n)
    if (exists(paste(n,k),memory,inherits=FALSE))
        return(get(paste(n,k),memory))
    updates<-P_12(n-1,k)+ P_12(n-1,k-1)
    assign(paste(n,k),updates,envir=memory)
    return(updates)
}

hash.table
A = c("A", "B", "C", "D")
A

'''
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::map<double, int> tableC(NumericVector x) {
    std::map<double, int> counts;
    
    int n = x.size();
    for (int i = 0; i < n; i++) {
        counts[x[i]]++;
    }
    
    return counts;
', pulgin="Rcpp")
}
'''
library(Rcpp)

runi <- cxxfunction(signature(number="numeric",alpha="numeric", beta="numeric"),'
                    int n = as<int>(number);
                    double a = as<double>(alpha);
                    double b = as<double>(beta);
                    RNGScope scope;
                    
                    NumericVector res = runif( n, a, b ) ;
                    return res ;
                    ', plugin="Rcpp" )



sourceCpp("table.cpp")
tableC(c(1,2,2,3))


characters <- NULL
for(i in 1:10000) {
characters <- rbind(characters, paste(LETTERS [as.integer( runif(200,1,4.99))], collapse="")) 
}

dt <- rbind(dt, dt)
dt <- rbind(dt, dt)
dt <- rbind(dt, dt)
write.csv(dt, "dt.csv")

str(characters)

# 모든 문자열의 첫번째 위치의 문자만 모아서 테이블 만들기
table(substr(characters,1,1))



# 1~200개의 결과를 리스트로 통합

lists <- NULL
for (i in 1:200){
lists <- c(lists, list(table(substr(characters,i,i))))
}

lists


col_num <- sprintf("%3d", 1:200)
testHash <- new.env(hash = TRUE, size = 10000000L)
for(i in 1:length(col_num)) {
    key <- col_num[i]
    assign(key, i, envir = testHash)
}

get(sprintf("%3d", 1), envir = testHash)

### HASH_TABLE
HASH_TABLE <- function() new.env()
INSERT <- function(key, value, ht)  ht[[key]] <- value
LOOKUP <- function(key, ht) ht[[key]]
DELETE <- function(key, ht) rm(list=key,envir=ht,inherits=FALSE)

ht <- HASH_TABLE()
INSERT("key",1L,ht)
INSERT("foo",1L,ht)
ls.str(ht)

LOOKUP("foo",ht)==1L
ht[["foo"]]==2L
