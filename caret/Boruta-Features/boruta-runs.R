setwd("C:/Kaggle/2015/Plankton/caret/Boruta-Features/")

library(Boruta)
SEED <- 713
set.seed(SEED)
RUNS <- 250

sink(paste0("Boruta-", RUNS, "-", SEED, ".txt"), split=TRUE)

time.1 <- Sys.time()
format(time.1, "%Y-%m-%d-%H%M%S")

load("../../Features/plankton-train-wndchrm-skimage-features.Rdata", verbose=TRUE)
length(train.class)
dim(train.features)

colnames(train.features)[2894]       # last wndchrm feature
colnames(train.features)[2895:2923]  # skimage features
train.features <- train.features[,-2895:-2923]
dim(train.features)

sum(is.na(train.features))

SELECT <- (runif(length(train.class)) > 0.97)
sum(SELECT)
train.class <- train.class[SELECT]
train.features <- train.features[SELECT,]

train.features <- data.frame(train.features)

borPlankton <- Boruta(train.class ~ train.features, doTrace=2, maxRuns=RUNS)
selected <- getSelectedAttributes(borPlankton)
length(selected)

table(borPlankton$finalDecision)

write.csv(data.frame(borPlankton$finalDecision),
          paste0("Boruta-", RUNS, "-", SEED, ".csv"))

save(borPlankton, file=paste0("Boruta-", RUNS, "-", SEED, ".Rdata"))

pdf(paste0("Boruta-", RUNS, "-", SEED, ".pdf"))
plot(borPlankton)
plotImpHistory(borPlankton)
dev.off()

time.2 <- Sys.time()
format(time.2, "%Y-%m-%d-%H%M%S")

sink()
