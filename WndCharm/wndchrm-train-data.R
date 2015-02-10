# Load wndchrm feature files for training cases and save to .Rdata file.
# efg, 2015-02-09

setwd("C:/Kaggle/2015/Plankton/Wnd-Charm/")

sink("wndchrm-train-data.txt", split=TRUE)
print(format(Sys.time(), "%Y-%m-%d-%H%M%S"))

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

save(feature.matrix, file=paste0(TRAIN.BINARY, "/plankton-train.Rdata"))

################################################################################

print(format(Sys.time(), "%Y-%m-%d-%H%M%S"))
sink()

