# Preparing data set
ds = read.csv("C:\\Users\\Fernando\\Downloads\\gwh.csv")
#subsetting only males data, weight and height
dsm = ds[ds[1]=="Male",]
dsm = subset(dsm, select=-c(Gender,Height, Weight))

model = lm(dsm[,1]~dsm[,2], dsm)
#predicts Height based on Weight
model
lmfunc = model$coefficients
lmfunc[2]

#V(dsm,)

V <- function(dset, model, boots, samplesize){
  statements
  return(object)