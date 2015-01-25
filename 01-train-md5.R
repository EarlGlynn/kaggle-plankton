# Inventory of Plankton train data:  md5s
# Use md5s to see if any images are in train and test sets.
# efg, 2015-01-24

#setwd("E:/Kaggle/2015/Plankton/")
setwd("C:/Users/Earl/Desktop/Kaggle/Plankton/")

filename <- paste0("01-train-md5-log-",
                   format(Sys.time(), "%Y-%m-%d-%H%M"), ".txt")
sink(filename, split=TRUE)

library(tools)     # md5sum
library(dplyr)     # filter

time.1 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

BASE.DIR <- "train/"

###############################################################################

train.dir   <- read.csv("00-Plankton-Train-Directories.csv", as.is=TRUE)
train.file  <- read.csv("00-Plankton-Train-FileList.csv", as.is=TRUE)

train.file$md5 <- ""

for (i in 1:nrow(train.file))
{
  filename <- paste0(BASE.DIR,
                     train.dir$species[train.file$species[i]], "/",
                     train.file$filename[i], ".jpg")
  if (i %% 1000 == 1)
  {
    cat(i, filename, "\n")
    flush.console()
  }

  train.file$md5[i] <- md5sum(filename)
}

write.csv(train.file, "01-Plankton-Train-FileList-MD5.csv", row.names=FALSE)

nrow(train.file)
length(unique(train.file$filename))
length(unique(train.file$md5))

### Duplicates

counts <- table(train.file$md5)
duplicates <- counts[counts > 1]

train.file %>% filter(md5 %in% names(duplicates))

###############################################################################

time.2 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

print(time.2 - time.1)
cat(sprintf(" %.1f", as.numeric(difftime(time.2, time.1,  units="secs"))), " secs\n")

sink()

