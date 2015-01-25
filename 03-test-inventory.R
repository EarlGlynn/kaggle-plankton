# Inventory of Plankton test data
# efg, 2014-12-16

#setwd("E:/Kaggle/2015/Plankton/")
setwd("C:/Users/Earl/Desktop/Kaggle/Plankton/")

library(magrittr)

filename <- paste0("03-test-inventory-log-",
                   format(Sys.time(), "%Y-%m-%d-%H%M"), ".txt")

sink(filename, split=TRUE)

time.1 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

BASE.DIR <- "test/"

test.files <- NULL

files <- list.files(path=BASE.DIR, full.names=TRUE)
info <- file.info(files)

short <- info %>% row.names() %>% basename()
filenames <- unlist(lapply(strsplit(short, "\\."), "[", 1))
filelist <- data.frame(filename=filenames,
                       filesize=info$size,
                       stringsAsFactors=FALSE)

write.csv(filelist, "03-Plankton-Test-FileList.csv", row.names=FALSE)
nrow(filelist)
length(unique(filelist$filename))

time.2 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

print(time.2 - time.1)
cat(sprintf(" %.1f", as.numeric(difftime(time.2, time.1,  units="secs"))), " secs\n")

sink()

