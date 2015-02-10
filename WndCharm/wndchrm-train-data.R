# Load wndchrm feature files for training cases and save to .Rdata file.
# efg, 2015-02-09

setwd("C:/Kaggle/2015/Plankton/Wnd-Charm/")

sink("wndchrm-train-data.txt", split=TRUE)
time.1 <- Sys.time()
print(format(time.1, "%Y-%m-%d-%H%M%S"))

TRAIN.BASE   <- "train"
TRAIN.BINARY <- "train-binary"

################################################################################
### Parse wndchrm file

parseWndchrm <- function(wndchrm)
{
  # For now, be very strict with header
  header   <- wndchrm[1]
  #stopifnot(header == "2\t3.2")

  filename <- wndchrm[2]

  wndchrm <- wndchrm[-1:-2]
  splits <- strsplit(wndchrm, "\t")

  values   <- as.numeric(unlist(lapply(splits, "[", 1)))
  features <- unlist(lapply(splits, "[", 2))

  invisible( list(header=header, filename=filename, values=values, features=features) )
}

################################################################################
### Count files in all TRAIN.BASE diretories

dirs <- list.dirs(path=TRAIN.BASE, full.names=FALSE, recursive=FALSE)

file_counts <- integer(length(dirs))
for (k in 1:length(dirs))
{
  directory <- dirs[k]
  files <- list.files(path=paste0(TRAIN.BASE, "/", directory),
                      pattern="^.*\\.sig$")
  file_counts[k] <- length(files)
}

Nfiles <- sum(file_counts)
stopifnot(Nfiles == 30336)  # Make sure all files counted

################################################################################
### Create empty feature.matrix

# Setup feature matrix based on first .sig file in first directory

directory <- dirs[1]
files <- list.files(path=paste0(TRAIN.BASE, "/", directory),
                    pattern="^.*\\.sig$")

# Read wndchrm file but ignore first two rows for now.
wndchrm <- readLines(paste0(TRAIN.BASE, "/", directory, "/", files[1]))
parsed <- parseWndchrm(wndchrm)

# Allocate feature matrix:
# One row for each file.  Once column for each feature,
# plus a column for the plankton class.
feature.matrix <- matrix(0, nrow=Nfiles, ncol=1+length(parsed$features))
dim(feature.matrix)
object.size(feature.matrix)

rowNames    <- rep("", Nfiles)
columnNames <- c("class", parsed$features) # R accepts almost any string as column name

################################################################################
### Loop through files and fill feature matrix

rowIndex <- 1
for (k in 1:length(dirs))
{
  directory <- dirs[k]
  files <- list.files(path=paste0(TRAIN.BASE, "/", directory),
                      pattern="^.*\\.sig$")

  cat(k, directory, length(files), "\n")
  flush.console()

  rowNames[rowIndex:(rowIndex+length(files)-1)] <- files

  for (i in 1:length(files))
  {
    wndchrm <- readLines(paste0(TRAIN.BASE, "/", directory, "/", files[i]))
    parsed <- parseWndchrm(wndchrm)
    stopifnot(all(columnNames[-1] == parsed$features)) # check column names
    feature.matrix[rowIndex,] <- c(k, parsed$values)
    rowIndex <- rowIndex + 1
  }

}

dimnames(feature.matrix) = list(rowNames, columnNames)
table(feature.matrix[,1])

################################################################################
### Remove constant columns.  How is that possible?
min.col <- apply(feature.matrix, 2, min)
max.col <- apply(feature.matrix, 2, max)
N.removed <- sum(min.col == max.col)

which(min.col == max.col)

constant.columns <- feature.matrix[,min.col == max.col]
head(constant.columns)
tail(constant.columns)

dim(feature.matrix)
N.removed
feature.matrix <- feature.matrix[,min.col != max.col]
dim(feature.matrix)

################################################################################
### Save file

print(format(Sys.time(), "%Y-%m-%d-%H%M%S"))
save(feature.matrix, file=paste0(TRAIN.BINARY, "/plankton-train.Rdata"))
print(format(Sys.time(), "%Y-%m-%d-%H%M%S"))

################################################################################
### SVD analysis; variance explained by first 50 eigenvalues

scaled <- scale(feature.matrix[, -1])
dim(scaled)

svd1 <- svd(scaled)
dim(svd1$u)
length(svd1$d)
dim(svd1$v)

# For now, let's look at first 50
N.eigen <- 50
eigen <- svd1$d[1:N.eigen]

pdf("wndchrm-train-data-eigenvalues.pdf")
plot(eigen^2/sum(eigen^2),pch=19,
     main="First 50 eigenvalues",
     xlab="eigenvalue",
     ylab="Fraction of variance explained by eigenvalue")
grid()
mtext(paste0(nrow(scaled), " rows, ", ncol(scaled), " variables"))

plot(cumsum(eigen^2/sum(eigen^2)),pch=19,
     main="First 50 eigenvalues",
     xlab="eigenvalue",
     ylab="Cumulative fraction of variance explained")
grid()

boxplot(svd1$v[,1:N.eigen],
        main=paste("First", N.eigen, "right singular vectors"))

dev.off()

################################################################################

time.2 <- Sys.time()
print(format(time.2, "%Y-%m-%d-%H%M%S"))
cat(sprintf("%.1f", as.numeric(difftime(time.2, time.1, units="secs"))), " secs\n")

sink()

