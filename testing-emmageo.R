library(EMMAgeo)

# load data
load(url("http://userpage.fu-berlin.de/~soga/300/30100_data_sets/data_emma.RData"))

phi <- colnames(df.soil.samples)
X <- as.matrix(df.soil.samples) # convert data.frame object to matrix object
str(X)
X
