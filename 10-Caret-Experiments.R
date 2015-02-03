# Caret experiments with Python skimage region properties.
# efg, 2015-02-01

setwd("C:/Kaggle/2015/Plankton/")

filename <- paste0("10-Caret-Experiments-log-",
                   format(Sys.time(), "%Y-%m-%d-%H%M"), ".txt")
sink(filename, split=TRUE)

library(plyr)      # put here because of caret internal conflict with dplyr
library(dplyr)     # filter
library(caret)     # http://topepo.github.io/caret/index.html

time.1 <- Sys.time()
cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

################################################################################
### Setup parallel processing

set.seed(19937)

# Let's use 6 cores on an i7-4770K 3.50 GHz processor with 32 GB RAM

library(doParallel)
rCluster <- makePSOCKcluster(6)  # Use 6 cores
registerDoParallel(rCluster)

###############################################################################

rawTrain  <- read.csv("train-properties-MASTER.csv", as.is=TRUE, row.names="filename")
str(rawTrain)
dim(rawTrain)

zeroRecords <- rawTrain %>% filter(area == 0)
dim(zeroRecords)

rawTrain <- rawTrain         %>%
            filter(area > 0) %>%
            select(-box_max_col, -box_max_row,
                   -box_min_col, -box_min_row,
                   -centroid_col, -centroid_row, -label)

# Classifier variable needs to be a factor
rawTrain$train <- factor(rawTrain$train)

dim(rawTrain)

################################################################################
# Remove near-zero variance variables from rawTrain.
nzv <- nearZeroVar(rawTrain, saveMetrics=TRUE)
count.nzv <- sum(nzv$nzv)
count.nzv
if (count.nzv > 0)
{
  rawTrain  <- rawTrain[,  !nzv$nzv]
}

################################################################################
pdf("10-Caret-Experiments-correlation-matrix.pdf")

# Remove variables with high correlation to others
HIGH.CORRELATION.CUTOFF <- 0.95
cor.matrix <- cor(rawTrain[,-ncol(rawTrain)])
heatmap(cor.matrix)

cor.high   <- findCorrelation(cor.matrix, HIGH.CORRELATION.CUTOFF)
high.cor.remove <- row.names(cor.matrix)[cor.high]
high.cor.remove
length(high.cor.remove)

rawTrain <- rawTrain[, -cor.high]
names(rawTrain)[-ncol(rawTrain)]
heatmap(cor(rawTrain[, -ncol(rawTrain)]))

dev.off()

################################################################################
# Partition raw training data into a training and validation set.
# Final testing set is a separate file.

TRAIN.PERCENT <- 0.75
inTrainSetIndex <- createDataPartition(y=rawTrain$train, p=TRAIN.PERCENT, list=FALSE)

training   <- rawTrain[ inTrainSetIndex,]
dim(training)
training <- training[complete.cases(training),]   # ignore problem data for now
dim(training)

validation <- rawTrain[-inTrainSetIndex,]
dim(validation)
validation <- validation[complete.cases(validation),]

################################################################################
# Apply specified caret method (lda) with specified preprocessing

#PREPROCESS <- c("center", "scale")
#METHOD <- "lda"
#
#PREPROCESS <- c()
#METHOD <- "rf"
#METHOD <- "rpart"
#METHOD <- "treebag"
#
#time.1 <- Sys.time()
#fit <- train(train ~ ., data = training,
#             preProcess=PREPROCESS, method=METHOD)
#
#OutOfSample  <- predict(fit, newdata=validation)
#confusion <- confusionMatrix(validation$train, OutOfSample)
#print(confusion)
#time.2 <- Sys.time()
#round(time.2 - time.1, 2)
#pdf(paste0("dotplot-", METHOD, ".pdf"))
#dotPlot(varImp(fit), main="lda:  Dotplot of variable importance values")
#dev.off()
#
sink()

################################################################################
# Use function to apply to a number of caret methods
# http://topepo.github.io/caret/modelList.html

# Use "sink" to save info to .txt file.
use.caret <- function(INFO, METHOD, PREPROCESS=NULL)
{
  sink(paste0("caret-", INFO, "-", METHOD, ".txt"), split=TRUE)
  print(INFO)
  print(METHOD)
  print(PREPROCESS)
  time.1 <- Sys.time()
  fit <- train(train ~ ., data = training,
               preProcess=PREPROCESS, method=METHOD)
  OutOfSample  <- predict(fit, newdata=validation)
  confusion <- confusionMatrix(validation$activity, OutOfSample)
  print(confusion)
  time.2 <- Sys.time()
  print(round(time.2 - time.1, 2))
  print(varImp(fit))
  sink()
  invisible(fit)
}

#fit.glm       <- use.caret("Generalized Linear Model",            "glm")

fit.lda       <- use.caret("Linear-Discriminant-Analysis",        "lda", PREPROCESS=c("center", "scale"))
fit.rpart     <- use.caret("Classification-and-Regression-Trees", "rpart")
fit.treebag   <- use.caret("BaggedCART",                          "treebag")

fit.rf        <- use.caret("Random-Forest",                       "rf")
fit.svmPoly   <- use.caret("Support-Vector-Machine-Polynomial",   "svmPoly")
fit.gbm       <- use.caret("Stochastic-Gradient-Boosting",        "gbm")

fit.bagFDA    <- use.caret("Bagged-Flex-Discriminant-Analysis",   "bagFDA")

fit.nb        <- use.caret("Naive-Bayes",                         "nb")

stopCluster(rCluster)
