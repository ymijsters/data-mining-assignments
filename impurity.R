library(data.tree)

##
# We use the gini-index as described. This is the R-translation within our structure.
# x: the vector of values of which the impurity should be determined
# Given a node x, we determine the impurity with formula i(x) = p(0|x)p(1|x) (two-class Gini-index).
# This function does that by counting the 0 or 1 classes and calculating the frequency and then using the formula.
##
tree.impurity <- function(x){
  if(length(x) == 0){
    return(0)
  }
  zero_frequency <- length(x[x == 0])/length(x)
  one_frequency <- length(x[x == 1])/length(x)
  return(zero_frequency * one_frequency)
}

##
# Calculates redistribution error with the formula given in the lecture notes
# x: all the trainingsamples for one variable and 
# y: is the classification of these samples
# Redistribution error is calculated using Rerror = i(x) - P(x | y = 0)*i(x | y=0) - P(x | y = 1)*i(x | y = 1) 
# With P being the partition and i the impurity
# return: the distribution error
##
redistribution <- function(x,y){
  currentImpurity <- tree.impurity(y)
  redis <- currentImpurity - (length(x[x==0])/length(x))*tree.impurity(y[which(x==0)]) - 
    (length(x[x==1])/length(x))*tree.impurity(y[which(x==1)])
  return(redis)
}

##
# Creates a comparison matrix for finding the best split.
# todo: elaborate
##
createCompMatrix <- function(x,y, minleaf){
  compmatrixx1 <- expand.grid(x,getsplits(x))
  splitmatrix <- matrix(as.numeric(compmatrixx1[,1] <= compmatrixx1[,2]), nrow=length(x))
  allowedSplitsWithMinLeaf = (which(colSums(splitmatrix) >= minleaf & colSums(splitmatrix) <= length(x) - minleaf))
  if(length(allowedSplitsWithMinLeaf) == 0 ){
    return(c(0,0))
  }
  splitmatrix <- matrix(splitmatrix[, allowedSplitsWithMinLeaf], ncol = length(allowedSplitsWithMinLeaf))
  impurity <- apply(splitmatrix,2, y=y, function(splitmatrix,y) redistribution(splitmatrix,y))
  return(c(getsplits(x)[which.max(impurity)],max(impurity)))
}
##
# Determines all possible splits for a single feature, by averaging every 2 consecutive values
#
# !!! Not working for multiclass feature classification and can be more efficient !!!
#
# feature: All trainingsamples for a feature. 
# return: All possible splits for feature or the only value if there are no available splits due to the minleaf constraint
# todo: elaborate
##
getsplits <- function(feature){
  if(length(unique(feature))>1){
    splitCalculationMatrix <- embed(unique(sort(feature)),2)
    return(rowMeans(splitCalculationMatrix))
  }
  else{
    return(unique(feature))
  }
}
##
# Determines a node of the tree by determining the split with the best redistribution
# data: dataset used for finding the optimal split in the current node
# class: classifications of trainingsamples present in the current node
# minleaf: the minimum amount of observations allowed in a node
# nfeat: the amount of features that should be looked at for each split
# return: a list with information about: value of the split, feature that is split, new dataset left and new dataset right
##
getnewnode <- function(data,class, minleaf, nfeat){
  selectedColumns <- sample(c(1:ncol(data)), min(nfeat,ncol(data)))
  datawithnfeat <- data[selectedColumns]
  result <- (apply(datawithnfeat,2,y=class, minleaf = minleaf, function(x,y, minleaf) createCompMatrix(x,t(y), minleaf)))
  print(result)
  print(data)
  bestsplitcolumn <- which.max(result[2,])
  bestsplit <-result[1,bestsplitcolumn]
  if(bestsplit == 0){
    return(list(0))
  }
  else{
    return(list(bestsplit, selectedColumns[bestsplitcolumn], c(which(data[selectedColumns[bestsplitcolumn]] <= bestsplit)), c(which(data[selectedColumns[bestsplitcolumn]] > bestsplit))))
  }
}


##
# Meat of the program recursive tree builder.
# todo: implement parameters
# The parameters are:
# data: the columns containing the dataset without the class that needs to be split on --> Should be called x
# class: the column containing only the class that needs to be split on --> Should be called y
# nmin: the minimum amount of observations in a node required to split
# minleaf: the minimum amount of observations allowed in a node
# nfeat: the amount of features that should be looked at for each split
##
build.tree <- function(data, class, nmin, minleaf, nfeat){
  #if there is impurity and there are enough observations for a split, then do so
  print(data)
  if(tree.impurity(class) > 0 && nrow(data) > nmin && ncol(data) > 0){
    result1 <- getnewnode(data,class, minleaf, nfeat)
    #There can still be no split because of the nfeat and minleaf limitations
    if(result1[[1]] != 0){
      result <- Node$new(colnames(data)[result1[[2]]], split = result1[[1]], samples = rownames(data), featurecolumn = result1[[2]])
      newclass1 <- class[result1[[3]]]
      newdata1 <- data[result1[[3]],-result1[[2]], drop = FALSE]
      newclass2 <- class[result1[[4]]]
      newdata2 <- (data[result1[[4]],-result1[[2]], drop = FALSE])
      #Add child nodes from the split and recursively continue
      result$AddChildNode(build.tree(newdata1, newclass1, nmin, minleaf, nfeat))
      result$AddChildNode(build.tree(newdata2, newclass2, nmin, minleaf, nfeat))
      return(result)
    }
  }
  return(Node$new(paste("Leaf", paste(rownames(data), collapse=",")), split= "Leaf", samples= rownames(data), class=class))
}
##
# Classify a trainingssample on a decision tree
##
tree.classify <- function(x, tree){
  
}


##Debug code --> nfeat and nmin work, minleaf not always
for(i in 1:1000){
  nfeat <- sample(1:5,1)
  minleaf <- sample(1:5,1)
  nmin <- sample(1:5,1)
  print(build.tree(credit.dat[1:5],t(credit.dat[6]), nmin, minleaf, nfeat), "split", "samples", "featurecolumn", "class")
}