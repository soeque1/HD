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
    print(i)    
    Hash[[i]] <- new.env(hash = TRUE, size = 10000L)
    
    for (j in 1:sum(hash.table[,1] == i))
    {
        key <- hash.table[hash.table[,1] == i,2][j]
        assign(key, j, envir = Hash[[i]])
    }
}


####### Data Load ##########
library(data.table)
dt <- fread("characters.csv")

for (i in 1:10)
{
dt <- rbind(dt, dt)
}
dim(dt)

####### Make Table ##########
system.time(a <- mget(substr(dt[[2]],1,1), envir = Hash[[1]]))

sapply(substr(dt[[2]],1,1), function(x) get(x, envir = Hash[[1]]))

system.time(unlist(mget(substr(dt[[2]],1,1), envir = Hash[[1]])))
system.time(tableC(unlist(mget(substr(dt[[2]],1,1), envir = Hash[[1]]))))

for (i in 1:200)
{
    print(table(substr(dt[[2]],i,i)))
}


library(Rcpp)
sourceCpp("table.cpp")
for (i in 1:200)
{
print(tableC(unlist(mget(substr(dt[[2]],i,i), envir = Hash[[i]]))))
}