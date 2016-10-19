rm(list = ls())

pkg.list = available.packages()
if(length(list.files('D:/rpkgs'))==0)
{
  download.packages(pkgs = pkg.list, destdir = "D:\\rpkgs")
} else {
  
  library(stringr)
  
  recent.pkgs <- str_match(list.files('D:/rpkgs'), '([a-zA-Z]+)')[,2]
  down.list <- rbind(pkg.list[pkg.list[,1] %in% recent.pkgs[length(recent.pkgs)], ], 
                     pkg.list[!pkg.list[,1] %in% recent.pkgs, ])
  
  download.packages(pkgs = down.list, destdir = "D:\\rpkgs")
}

#apply(pkg.list[,1:2], 1, function(x) paste0(paste0(x, collapse = "_"), '')
#install.packages("C:/Users/KCB/Downloads/R/gbm_2.1.1.tar.gz", repos = NULL, type = "source")


