# Load wndchrm features for training cases and explore.
# efg, 8 Feb 2015

TRAIN.BASE <- "train"

setwd("C:/Kaggle/2015/Plankton/Wnd-Charm/")

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

pdf("wndchrm-explorations.pdf")

################################################################################
### Create empty feature.matrix

dirs <- list.dirs(path=TRAIN.BASE, full.names=FALSE, recursive=FALSE)

for (k in 1:length(dirs))
{
  directory <- dirs[k]

  files <- list.files(path=paste0(TRAIN.BASE, "/", directory),
                      pattern="^.*\\.sig$")

  cat(directory, length(files), "\n")
  flush.console()

  # Setup feature matrix based on first .sig file in directory
  i <- 1

  # Read wndchrm file but ignore first two rows for now.
  wndchrm <- readLines(paste0(TRAIN.BASE, "/", directory, "/", files[i]))
  parsed <- parseWndchrm(wndchrm)

  # Allocate feature matrix and name rows and columns
  feature.matrix <- matrix(0, nrow=length(files), ncol=length(parsed$features))
  rownames <- files
  colnames <- parsed$features # R accepts almost any string as column name
  dimnames(feature.matrix) = list(rownames, colnames)
  feature.matrix[1:3,1:4]

  ################################################################################
  ### Loop through files and fill feature matrix
  for (i in 1:length(files))
  {
    #cat(files[i], "\n")
    stopifnot(rownames(feature.matrix)[i] == files[i])          # check row names
    wndchrm <- readLines(paste0(TRAIN.BASE, "/", directory, "/", files[i]))
    parsed <- parseWndchrm(wndchrm)
    stopifnot(all(colnames(feature.matrix) == parsed$features)) # check column names
    feature.matrix[i,] <- parsed$values
  }
  feature.matrix[,1:4]

  min.col <- apply(feature.matrix, 2, min)
  max.col <- apply(feature.matrix, 2, max)
  N.removed <- sum(min.col == max.col)

  # Scale only non-constant columns

  dim(feature.matrix)
  scaled <- scale(feature.matrix[, min.col != max.col])
  dim(scaled)

  svd1 <- svd(scaled)
  dim(svd1$u)
  length(svd1$d)
  dim(svd1$v)

  par(mfrow=c(2,2))

  plot(svd1$d^2/sum(svd1$d^2),pch=19, main=directory,
       xlab="eigenvalue",
       ylab="Fraction of variance explained")
  grid()
  mtext(paste0(length(files), " files, ", ncol(scaled), " variables (",
              N.removed, " removed)"))

  plot(cumsum(svd1$d^2/sum(svd1$d^2)),pch=19, main=directory,
       xlab="eigenvalue",
       ylab="Cumulative fraction of variance explained")
  grid()

  boxplot(svd1$v[,1:10], main="First 10 right singular vectors")
}

################################################################################
dev.off()
