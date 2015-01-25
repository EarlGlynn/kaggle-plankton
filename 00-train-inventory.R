# Inventory of Plankton train data
# efg, 2014-12-16

#setwd("E:/Kaggle/2015/Plankton/")
setwd("C:/Users/Earl/Desktop/Kaggle/Plankton/")

library(magrittr)

filename <- paste0("00-train-inventory-log-",
                   format(Sys.time(), "%Y-%m-%d-%H%M"), ".txt")

sink(filename, split=TRUE)

time.1 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

BASE.DIR <- "train/"

species <- list.dirs(path=BASE.DIR, recursive=FALSE, full.names=FALSE)
N <- length(species)

summary.info <- data.frame(directory=1:N,
                           species=species, count=rep(0,N),
                           min=rep(0,N),
                           median=rep(0,N),
                           max=rep(0,N),
                           stringsAsFactors=FALSE)

filesize.list <- vector("list", N)

train.files <- NULL

for (i in 1:N)
{
  # summary.info
  files <- list.files(path=paste0(BASE.DIR, species[i]), full.names=TRUE)
  info <- file.info(files)
  summary.info$count[i] <- nrow(info)
  cat(species[i], summary.info$count[i], "\n")
  flush.console()

  summary.info$min[i]    <- min(info$size)
  summary.info$median[i] <- median(info$size)
  summary.info$max[i]    <- max(info$size)

  filesize.list[[i]] <- info$size
  names(filesize.list[[i]]) <- species[i]

  # filename - species list
  short <- info %>% row.names() %>% basename()
  filenames <- unlist(lapply(strsplit(short, "\\."), "[", 1))
  filelist <- data.frame(filename=filenames,
                         species=rep(i, nrow(info)),
                         filesize=info$size,
                         stringsAsFactors=FALSE)
  train.files <- rbind(train.files, filelist)
}

write.csv(summary.info, "00-Plankton-Train-Directories.csv", row.names=FALSE)

# verify all filenames are unique across train species
write.csv(train.files,  "00-Plankton-Train-FileList.csv", row.names=FALSE)
nrow(train.files)
length(unique(train.files$filename))
stopifnot(nrow(train.files) == length(unique(train.files$filename))    )

table(train.files$species)

png("00-Plankton-File-Size.png", width=2048, height=800)

boxplot(filesize.list, las=2, main="Plankton File Sizes",
        ylab="File Size [bytes]",
        at=rank(summary.info$median, ties.method="random"))
dev.off()

time.2 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

print(time.2 - time.1)
cat(sprintf(" %.1f", as.numeric(difftime(time.2, time.1,  units="secs"))), " secs\n")

sink()

