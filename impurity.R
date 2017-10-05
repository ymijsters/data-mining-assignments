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
  uniquex = unique(as.vector(x))
  return(sum(unlist(lapply(uniquex, classes = x, function(x, classes) (length(classes[which(classes==x)])/length(classes))*(length(classes[which(classes!=x)])/length(classes))))))
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
  possiblesplits <- getsplits(x, y, minleaf)
  compmatrixx1 <- expand.grid(x, possiblesplits)
  splitmatrix <- matrix(as.numeric(compmatrixx1[,1] <= compmatrixx1[,2]), nrow=length(x))
  allowedSplitsWithMinLeaf = (which(colSums(splitmatrix) >= minleaf & colSums(splitmatrix) <= length(x) - minleaf))
  if(length(allowedSplitsWithMinLeaf) == 0 ){
    return(c(0,0))
  }
  splitmatrix <- matrix(splitmatrix[, allowedSplitsWithMinLeaf], ncol = length(allowedSplitsWithMinLeaf))
  impurity <- apply(splitmatrix,2, y=y, function(splitmatrix,y) redistribution(splitmatrix,y))
  return(c(possiblesplits[allowedSplitsWithMinLeaf[which.max(impurity)]],max(impurity)))
}
##
# Determines all possible splits for a single feature, by averaging every 2 consecutive values
# If minleaf = 0, this can be done more efficient by determining segments and only considering segment splits
# feature: All trainingsamples for a feature. 
# return: All possible splits for feature or the only value if there are no available splits due to the minleaf constraint
# todo: elaborate
##
getsplits <- function(feature, classes, minleaf){
  if(length(unique(feature))>1){
    if(minleaf > 0 || unique(feature) < 3){
      splitCalculationMatrix <- embed(unique(sort(feature)),2)
      return(rowMeans(splitCalculationMatrix))
    }
    else{
      uniques = sort(unique(feature))
      segmentmatrix <- (sapply(uniques, class = classes, feature=feature, function(x, feature, class) c(x,mean(class[which(feature==x)]))))
      return(rowMeans(matrix(c(uniques[which(embed(segmentmatrix[2,],2)[,1]!=embed(segmentmatrix[2,],2)[,2])],uniques[which(embed(segmentmatrix[2,],2)[,1]!=embed(segmentmatrix[2,],2)[,2])+1]), ncol=2)))
    }
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
  if(tree.impurity(class) > 0 && nrow(data) > nmin && ncol(data) > 0 && nrow(data)/2 >= minleaf){
    result1 <- getnewnode(data,class, minleaf, nfeat)
    #There can still be no split because of the nfeat and minleaf limitations
    if(result1[[1]] != 0){
      result <- Node$new(colnames(data)[result1[[2]]], split = result1[[1]], samples = rownames(data), featurecolumn = result1[[2]])
      newclass1 <- class[result1[[3]]]
      newdata1 <- data[result1[[3]], -result1[[2]], drop = FALSE]
      newclass2 <- class[result1[[4]]]
      newdata2 <- data[result1[[4]], -result1[[2]], drop = FALSE]
      #Add child nodes from the split and recursively continue
      child1 <- build.tree(newdata1, newclass1, nmin, minleaf, nfeat)
      child2 <- build.tree(newdata2, newclass2, nmin, minleaf, nfeat)
      child1$name <- paste(child1$name, " ", child1$split, " 1")
      child2$name <- paste(child2$name, " ", child2$split, " 2")
      result$AddChildNode(child1)
      result$AddChildNode(child2)
      return(result)
    }
  }
  return(Node$new(paste("Leaf", paste(c(sum(class==0),sum(class==1)), collapse=",")), split= "Leaf", samples= paste(rownames(data), collapse=","), class=paste(class, collapse=",")))
}
##
# y : classes of samples in a leaf node
# return: Most likely class of sample in leaf node
##
averageclasses <- function(y){
  y = unlist(lapply(strsplit(y,","), as.integer))
  classes= unique((y))
  return(classes[which.max(tabulate(match(y,classes)))])
}

tree.getclassification <- function(x, tree){
  if(tree$split == 'Leaf'){
    return(averageclasses(tree$class))
  }
  else if(x[tree$featurecolumn] <= tree$split){
    return(tree.getclassification(x, tree$children[[1]]))
  }
  else{
    return(tree.getclassification(x, tree$children[[2]]))
  }
}

##
# Classify a trainingssample on a decision tree
##
tree.classify <- function(x, tree){
  return(apply(x,1,tree=tree, function(x, tree) tree.getclassification(x,tree)))
}
##
# Get the precision: The percentage correctly classified positives of the total positives of the classificationset 
##
getPrecision <- function(ymodel, ydata){
  print(length(which(ymodel==1 & ydata==1))/length(which(ymodel==1)))
}
##
# Get the accuracy: The percentage correctly classified positives of the total positives of the testset
##
getRecall <- function(ymodel, ydata){
  print(length(which(ymodel==1 & ydata==1))/length(which(ydata==1)))
}
##
# Get the accuracy: The percentage correctly classified samples of samples in the dataset
##
getAccuracy <- function(ymodel, ydata){
  print((length(which(ymodel==1 & ydata==1))+length(which(ymodel==0 & ydata==0)))/length(ymodel))
}
##
# Excercise 2a
##
secondtree <- build.tree(trainingset[c(3, 5:44)], as.numeric(dataset[4] > 0), 15, 5, 41)
classifications <- tree.classify(testset[c(3,5:45)], secondtree)
getPrecision(classifications, as.numeric(testset[4] > 0))
getRecall(classifications, as.numeric(testset[4] > 0))
getAccuracy(classifications, as.numeric(testset[4] > 0))