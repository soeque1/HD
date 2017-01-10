rm(list = ls())

ext.down.pkgs <- function()
{
  pkg.url <- 'https://github.com/qinwf/awesome-R'
  library(rvest)
  h <- read_html(pkg.url)
  pkg.list <- h %>% html_nodes(' ul li') %>% html_nodes('a') %>% html_text()
  start.pkg <- which(pkg.list == 'magrittr')
  end.pkg <- which(pkg.list == 'swirl')
  
  pkg.hot.list <- pkg.list[start.pkg:end.pkg]
  pkg.hot.list
}

pkg.hot.list <- ext.down.pkgs()
# 
# ## Dependencies
# library(tools)
# #which(pkg.list[,1] == 'ggplot2')
# #pkg.list[which(pkg.list[,1] == 'ggplot2'),]
# 
# ext.dependent.pkgs <- function(pkg.list.import = pkg.list[,5])
# {
#   pkg.dep.list <- str_replace_all(pkg.list.import, '\\(>=[[:space:]]?[0-9]+\\.[0-9]+\\.[0-9]+\\)', '')
#   pkg.dep.list <- str_replace_all(pkg.dep.list, '\\(>=[[:space:]]?[0-9]+\\.[0-9]+\\)', '')
#   pkg.dep.list <- str_replace_all(pkg.dep.list, '\\\n', '')
#   pkg.dep.list <- str_replace_all(pkg.dep.list, '\\(>=[[:space:]]?[0-9]+\\.[0-9]+-[0-9]+\\)', '')
#   pkg.dep.list <- str_replace_all(pkg.dep.list, '\\(>=[[:space:]]?[0-9]+\\.[0-9]+-[0-9]+\\.[0-9]+\\)', '')
#   pkg.dep.list <- str_replace_all(pkg.dep.list, '\\(>=[[:space:]]?[0-9]+\\.[0-9]+\\.[0-9]+-[0-9]+\\)', '')
#   
#   pkg.dep.list <- sapply(pkg.dep.list, function(x) str_trim(str_split(x, ',')[[1]]), USE.NAMES = F)
#   pkg.dep.list <- unique(unlist(pkg.dep.list))
#   
#   ## Delete NA and R
#   pkg.dep.list <- str_replace_all(pkg.dep.list, 'R', '')
#   pkg.dep.list <- pkg.dep.list[!is.na(pkg.dep.list)]
#   pkg.dep.list <- pkg.dep.list[pkg.dep.list != ""]
#   
#   #pkg.dep.list <- sort(pkg.dep.list)
#   pkg.dep.list
# }

pkg.list <- available.packages()
pkg.list <- pkg.list[pkg.list[,1] %in% pkg.hot.list, ]
head(pkg.list)

down.dir <- 'D:/r_useful_pkgs/'
pkg.list.sav <- pkg.list
rownames(pkg.list.sav) <- 1:nrow(pkg.list.sav)
write.csv(pkg.list.sav, 'D:/r_useful_pkgs.csv')

# library(stringr)
# library('testthat')
# ## TEST - ggplot2
# expect_equal(c("digest", "grid", "gtable", "MASS", "plyr", "reshape2", "scales", "stats"),
#              ext.dependent.pkgs(pkg.list[55,5]))
# 
# pkg.dep.list <- ext.dependent.pkgs(pkg.list[,5])
# 
# pkg.list <- available.packages()
# 
# pkg.hot.list <- union(pkg.hot.list, pkg.dep.list)
# pkg.list <- pkg.list[pkg.list[,1] %in% pkg.hot.list, ]
# 
# dim(pkg.list)
# 
# if(length(list.files(down.dir))==0)
# {
#   download.packages(pkgs = pkg.list, destdir = down.dir)
# } else {
#   
#   library(stringr)
#   
#   recent.pkgs <- str_match(list.files(down.dir), '([a-zA-Z]+.+)_')[,2]
#   down.list <- rbind(pkg.list[pkg.list[,1] %in% recent.pkgs[length(recent.pkgs)], ], 
#                      pkg.list[!pkg.list[,1] %in% recent.pkgs, ])
#   
#   download.packages(pkgs = down.list, destdir = down.dir)
# }

library(miniCRAN)

repo <- 'http://cran.nexr.com'

pth <- file.path(down.dir)

length(pkg.hot.list)
dep_pkgs  <- pkgDep(pkg.hot.list, repos=repo, type = "win.binary")

dep_pkgs

makeRepo(dep_pkgs, path=pth, repos=repo, type = 'win.binary')

update.dir <- 'D:/r_useful_pkgs/'
updateRepoIndex(path=update.dir, type = 'win.binary')

#pkg.dir <- 'file:D:/r_useful_pkgs/src/contrib'
#install.packages('ggplot2', repos = NULL, type = 'win.binary', contriburl = pkg.dir, libs_only = T)

