# Inventory of Plankton test data:  md5s
# Use md5s to see if any images are in train and test sets.
# efg, 2015-01-24

#setwd("E:/Kaggle/2015/Plankton/")
setwd("C:/Users/Earl/Desktop/Kaggle/Plankton/")

filename <- paste0("04-test-md5-log-",
                   format(Sys.time(), "%Y-%m-%d-%H%M"), ".txt")
sink(filename, split=TRUE)

library(tools)     # md5sum
library(dplyr)     # filter
library(EBImage)   # readImage

time.1 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

BASE.DIR <- "test/"

###############################################################################

test.file  <- read.csv("03-Plankton-Test-FileList.csv", as.is=TRUE)

test.file$nrow <- 0
test.file$ncol <- 0
test.file$md5 <- ""

for (i in 1:nrow(test.file))
{
  filename <- paste0(BASE.DIR, test.file$filename[i], ".jpg")
  if (i %% 1000 == 1)
  {
    cat(i, filename, "\n")
    flush.console()
  }

  img <- readImage(filename)
  test.file$nrow[i] <- nrow(img)
  test.file$ncol[i] <- ncol(img)

  test.file$md5[i] <- md5sum(filename)
}

write.csv(test.file, "04-Plankton-Test-FileList-MD5.csv", row.names=FALSE)

nrow(test.file)
length(unique(test.file$filename))
length(unique(test.file$md5))

### Duplicates

counts <- table(test.file$md5)
table(counts)
duplicate.counts <- counts[counts > 1]

duplicates <- test.file                                %>%
              filter(md5 %in% names(duplicate.counts)) %>%
              arrange(md5, filename)
write.csv(duplicates, "04-Plankton-Test-FileList-Duplicates.csv", row.names=FALSE)

big.dups <- counts[counts > 6]
check.list <- test.file %>% filter(md5 %in% names(big.dups)) %>% arrange(md5)
write.csv(check.list, "04-Plankton-Test-FileList-BigDups.csv", row.names=FALSE)
###############################################################################
### Compare with train file

unique.test <- sort(unique(test.file$md5))
length(unique.test)

train.file <- read.csv("01-Plankton-Train-FileList-MD5.csv", as.is=TRUE)
unique.train <- sort(unique(train.file$md5))
length(unique.train)

in.both <- intersect(unique.train, unique.test)
train.file %>% filter(md5 %in% in.both)
test.file  %>% filter(md5 %in% in.both)

###############################################################################

time.2 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

print(time.2 - time.1)
cat(sprintf(" %.1f", as.numeric(difftime(time.2, time.1,  units="secs"))), " secs\n")

sink()

