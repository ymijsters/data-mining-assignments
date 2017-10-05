library(data.tree)

##
# Description
# 	Meat of the program recursive tree builder.
# Parameters
#	data: the columns containing the dataset without the class that needs to be split on --> Should be called x
# 	class: the column containing only the class that needs to be split on --> Should be called y
# 	nmin: the minimum amount of observations in a node required to split
# 	minleaf: the minimum amount of observations allowed in a node
# 	nfeat: the amount of features that should be looked at for each split
# Return
#	?
##
tree.grow <- function(x, y, nmin, minleaf, nfeat){
  #if there is impurity and there are enough observations for a split, then do so
  if(tree.impurity(y) > 0 && nrow(x) > nmin && ncol(x) > 0 && nrow(x)/2 >= minleaf){
    result1 <- tree.getNewNode(x,y, minleaf, nfeat)
    #There can still be no split because of the nfeat and minleaf limitations
    if(result1[[1]] != 0){
      result <- Node$new(colnames(x)[result1[[2]]], split = result1[[1]], samples = rownames(x), featurecolumn = result1[[2]])
      newy1 <- y[result1[[3]]]
      newx1 <- x[result1[[3]], -result1[[2]], drop = FALSE]
      newy2 <- y[result1[[4]]]
      newx2 <- x[result1[[4]], -result1[[2]], drop = FALSE]
      #Add child nodes from the split and recursively continue
      child1 <- tree.grow(newx1, newy1, nmin, minleaf, nfeat)
      child2 <- tree.grow(newx2, newy2, nmin, minleaf, nfeat)
      child1$name <- paste(child1$name, " ", child1$split, " 1")
      child2$name <- paste(child2$name, " ", child2$split, " 2")
      result$AddChildNode(child1)
      result$AddChildNode(child2)
      return(result)
    }
  }
  return(Node$new(paste("Leaf", paste(c(sum(y==0),sum(y==1)), collapse=",")), split= "Leaf", samples= paste(rownames(x), collapse=","), class=paste(y, collapse=",")))
}

##
# Description
# 	Classify a trainingssample on a decision tree. We get each row from the matrix and then transform that to a classifcation of the row.
# Parameters
# 	x: the dataset
# 	tree: the tree to classify with
# Return
# 	?
##
tree.classify <- function(x, tree){
  return(apply(x,1,tree=tree, function(x, tree) tree.getClassification(x,tree)))
}


##
# Description
# 	We use the gini-index as described. This is the R-translation within our structure.
# 	Given a vector x, we determine the impurity with formula i(x) = p(0|x)p(1|x) (two-class Gini-index).
# 	This function does that by counting the 0 or 1 classes and calculating the frequency and then using the formula.
# Parameters
# 	x: the vector of values of which the impurity should be determined
# Return
# 	The gini-index of the vector. Float. 0 < x < 1.
##
tree.impurity <- function(x){
  if(length(x) == 0){
    return(0)
  }
  uniquex = unique(as.vector(x))
  return(sum(unlist(lapply(uniquex, classes = x, function(x, classes) (length(classes[which(classes==x)])/length(classes))*(length(classes[which(classes!=x)])/length(classes))))))
}

##
# Description
# 	Calculates tree.redistribution error with the formula given in the lecture notes
# 	Redistribution error is calculated using Rerror = i(x) - P(x | y = 0)*i(x | y=0) - P(x | y = 1)*i(x | y = 1) 
# 	With P being the partition and i the impurity
# Parameters
# 	x: all the trainingsamples for one variable and 
# 	y: is the classification of these samples
# Return 
#	The distribution error. Float. 0 < x < 1
##

tree.redistribution <- function(x,y){
  currentImpurity <- tree.impurity(y)
  redis <- currentImpurity - (length(x[x==0])/length(x))*tree.impurity(y[which(x==0)]) - 
    (length(x[x==1])/length(x))*tree.impurity(y[which(x==1)])
  return(redis)
}

#TODO: explain further
##
# Description
#	Creates a comparison matrix for finding the best split.
# Parameters
#       x: all the trainingsamples for one variable and 
#       y: is the classification of these samples
#       minleaf: the minimum amount of observations allowed in a node
# Result
# 	Comparison matrix
##

createCompMatrix <- function(x,y, minleaf){
  possiblesplits <- tree.getSplits(x, y, minleaf)
  compmatrixx1 <- expand.grid(x, possiblesplits)
  splitmatrix <- matrix(as.numeric(compmatrixx1[,1] <= compmatrixx1[,2]), nrow=length(x))
  allowedSplitsWithMinLeaf = (which(colSums(splitmatrix) >= minleaf & colSums(splitmatrix) <= length(x) - minleaf))
  if(length(allowedSplitsWithMinLeaf) == 0 ){
    return(c(0,0))
  }
  splitmatrix <- matrix(splitmatrix[, allowedSplitsWithMinLeaf], ncol = length(allowedSplitsWithMinLeaf))
  impurity <- apply(splitmatrix,2, y=y, function(splitmatrix,y) tree.redistribution(splitmatrix,y))
  return(c(possiblesplits[allowedSplitsWithMinLeaf[which.max(impurity)]],max(impurity)))
}

#TODO: check
##
# Description
# 	Determines all possible splits for a single feature, by averaging every 2 consecutive values
# 	If minleaf = 0, this can be done more efficient by determining segments and only considering segment splits
# Parameters
#	feature: All trainingsamples for a feature. 
# Return 
#	All possible splits for feature or the only value if there are no available splits due to the minleaf constraint
##

tree.getSplits <- function(feature, classes, minleaf){
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
#TODO: check
##
# Description
# 	Determines a node of the tree by determining the split with the best redistribution
# Parameters 
#	data: dataset used for finding the optimal split in the current node
# 	class: classifications of trainingsamples present in the current node
# 	minleaf: the minimum amount of observations allowed in a node
# 	nfeat: the amount of features that should be looked at for each split
# Return: 
#	a list with information about: value of the split, feature that is split, new dataset left and new dataset right
##
tree.getNewNode <- function(data,class, minleaf, nfeat){
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
# Description
# 	Finds class within samples with highest frequencies
# Parameters 
# 	y : classes of samples in a leaf node
# Return
# 	Most likely class of sample in leaf node
##
tree.mostCommonClass <- function(y){
  #We change the comma-delimited line to a list of values
  normalizedY <- unlist(lapply(strsplit(y,","), as.integer))
  #Return the most common value
  return(names(sort(table(normalizedY),decreasing=TRUE)[1])[1])
}

tree.getClassification <- function(x, tree){
  if(tree$split == 'Leaf'){
    return(tree.mostCommonClass(tree$class))
  }
  else if(x[tree$featurecolumn] <= tree$split){
    return(tree.getClassification(x, tree$children[[1]]))
  }
  else{
    return(tree.getClassification(x, tree$children[[2]]))
  }
}



##
# Exercise 2a
##

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


dataset <- read.csv("eclipse-metrics-packages-2.0.csv", sep=";")
trainingset <- read.csv("eclipse-metrics-packages-2.1.csv", sep=";")
testset <- read.csv("eclipse-metrics-packages-3.0.csv", sep=";")
secondtree <- tree.grow(trainingset[c(3, 5:44)], as.numeric(dataset[4] > 0), 15, 5, 41)
classifications <- tree.classify(testset[c(3,5:45)], secondtree)
getPrecision(classifications, as.numeric(testset[4] > 0))
getRecall(classifications, as.numeric(testset[4] > 0))
getAccuracy(classifications, as.numeric(testset[4] > 0))
