library(tibble)
# Preparing data set
ds = read.csv("C:\\Users\\Fernando\\Downloads\\gwh.csv")
#subsetting only males data, weight and height
dsm = ds[ds[1]=="Male",]
dsm = subset(dsm, select=-c(Gender,Height, Weight))

names(dsm)[1] <- "H"
names(dsm)[2] <- "W"
dsm[10,]

#predicts Height based on Weight

Variance <- function(dset, model, nboo, samplesize){
  get_boot <- function(x){
  index <- sample(1:samplesize,
                  size = samplesize,
                  replace = TRUE)
  return (dset[index,])
  }
  
  boots <- lapply(1:nboo, get_boot) #saves all samples in boots
  
  fit_lm <- function(dset,degree=2){
    formula <- paste0("I(","W^",1:degree,")",collapse = '+')
    formula <- paste0("H ~ ",formula)
    fit <- lm(formula,data = dset)
    return(fit)
  }
  
  models = lapply(boots, fit_lm)
  #nboo models
  print(models)
  
  fixj <- predict(models)
  mean_fixj = rowMeans(fixj) #nboo rows, 1 col containing the Avg of predictions per x
  var_ij =  (colSums(fixj-meanfixj)^2)/(nboo-1)
  return (var_ij)
}

Variance(dsm,model1, nboo=3, samplesize = 30)

####################################################