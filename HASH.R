setwd("~/Dropbox/repo/HD")

library(data.table)
####### Data Load ##########
dt <- fread("characters.csv")

for (i in 1:10)
{
dt <- rbind(dt, dt)
}

h.a <- NULL
h.b <- NULL
for (i in 1:200)
{
h.a <- c(h.a, rep(i, 5))
h.b <- c(h.b, c("A", "B", "C", "D", "E"))
}

hash.table <- data.frame(h.a, h.b)

col_num <- sprintf("%3d", 1:200)
testHash <- new.env(hash = TRUE, size = 10000000L)
for(i in 1:length(col_num)) {
    key <- col_num[i]
    assign(key, i, envir = testHash)
}

get(sprintf("%3d", 1), envir = testHash)


Hash <- list()

for (i in 1:200)
{
print(i)    
Hash[[i]] <- new.env(hash = TRUE, size = 10000L)

for (j in 1:sum(hash.table[,1] == i))
{
    key <- hash.table[hash.table[,1] == i,2][j]
    assign(key, j, envir = Hash[[i]])
}
}

library(Rcpp)
sourceCpp("table.cpp")
for (i in 1:200)
{
print(tableC(unlist(mget(substr(dt[[2]],i,i), envir = Hash[[i]]))))
}






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
