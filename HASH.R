setwd("~/Dropbox/repo/HD")

####### Prepare ########
characters <- NULL
for(i in 1:10000) {
    characters <- rbind(characters, paste(LETTERS [as.integer( runif(200,1,4.99))], collapse="")) 
}
write.csv(characters, "characters.csv")

h.a <- NULL
h.b <- NULL
for (i in 1:200)
{
    h.a <- c(h.a, rep(i, 4))
    h.b <- c(h.b, c("A", "B", "C", "D"))
}

hash.table <- data.frame(h.a, h.b)

Hash <- list()

for (i in 1:200)
{
 #   print(i)   
    n <- as.integer(sum(hash.table[,1] == i))
    
    Hash[[i]] <- new.env(hash = TRUE, size = n)
    
    for (j in 1:n)
    {
        key <- hash.table[hash.table[,1] == i,2][j]
        assign(key, as.integer(j), envir = Hash[[i]])
    }
}

ls(Hash[[19]])
is.integer(get("A", Hash[[19]]))

####### Data Load ##########
library(data.table)
dt <- fread("characters.csv")

for (i in 1:11)
{
dt <- rbind(dt, dt)
}
dim(dt)

####### Make Table ##########
system.time(a <- mget(substr(dt[[2]],1,1), envir = Hash[[1]]))
system.time(as.integer(mget(substr(dt[[2]],1,1), envir = Hash[[1]])))

library(Rcpp)
sourceCpp("table.cpp")
system.time(tableC(as.integer(mget(substr(dt[[2]],1,1), envir = Hash[[1]]))))
system.time(table(substr(dt[[2]],1,1)))

table_list <- list()

for (i in 1:2)
{
table_list[[i]] <- table(substr(dt[[2]],i,i))
}

unlist(table_list)

for (i in 1:2)
{
table_list[[i]] <- tableC(as.integer(mget(substr(dt[[2]],i,i), envir = Hash[[i]])))
}

unlist(table_list)
