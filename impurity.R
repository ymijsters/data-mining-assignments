library(data.tree)

##
# We use the gini-index as described. This is the R-translation within our structure.
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
# Grows the tree
##
tree.grow <- function(x,y,nmin,minleaf,nfeat){
  nodelist <- x
  tree <- Node$new("test1")
  teller<-0
  for(i in x){
    if(impurity(t(y))>0){
      bestsplit<-calculate.best.split(i,y)
      tree<-tree$AddChild(teller)
      print(tree)
      teller<-teller+1
    }
  }
  print(tree)
}
##
# Calculates best split
# todo: describe
##
calculate.best.split <- function(x,y){
  currentImp = impurity(t(y))
  splitlist <- vector()
  split <- t(unique(x))
  if(length(x[x<=split]) > 0&& length(x[x>split])>0){
    splitlist <- c(splitlist, redistribution(x,y,split,currentImp))
  }
  print(max(t(max(splitlist))))
}

##
# Calculates redistribution error with the formula given in the lecture notes
# todo: define parameters and explain code
##
redistribution <- function(x,y){
  currentImp <- tree.impurity(y)
  redis <- currentImp - (length(x[x==0])/length(x))*tree.impurity(y[which(x==0)]) - 
    (length(x[x==1])/length(x))*tree.impurity(y[which(x==1)])
  return(redis)
}

##
# Creates a comparison matrix for finding the best split.
# todo: elaborate
##
createCompMatrix <- function(x,y){
  compmatrixx1 <- expand.grid(x,getsplits(x))
  splitmatrix <- matrix(as.numeric(compmatrixx1[,1] <= compmatrixx1[,2]), nrow=length(x))
  impurity <- apply(splitmatrix,2, y=y, function(splitmatrix,y) redistribution(splitmatrix,y))
  return(c(getsplits(x)[which.max(impurity)],max(impurity)))
}
##
# Splits a node depending on the classes. If the node is pure, then return the node. Otherwise split.
# todo: elaborate
##
getsplits <- function(node){
  if(length(unique(node))>1){
    m <- embed(unique(sort(node)),2)
    return(rowMeans(m))
  }
  else{
    return(unique(node))
  }
}

getnewnode <- function(data,class){
  result <- (apply(data,2,y=class,function(x,y) createCompMatrix(x,t(y))))
  bestsplitcolumn <- which.max(result[2,])
  bestsplit <-result[1,bestsplitcolumn]
  return(list(bestsplit, bestsplitcolumn, c(which(data[bestsplitcolumn] <= bestsplit)), c(which(data[bestsplitcolumn] > bestsplit))))
}


##
# Meat of the program.
# todo: implement parameters
# The parameters are:
# data: the columns containing the dataset without the class that needs to be split on
# class: the column containing only the class that needs to be split on
# nmin: the minimum amount of observations in a node required to split
# minleaf: the minimum amount of observations allowed in a node
# nfeat: the amount of features that should be looked at for each split
##
build.tree <- function(data, class, nmin, minleaf, nfeat){
  #if there is impurity and there are enough observations for a split, then do so
  if(tree.impurity(class) > 0 && nrow(data) > nmin){
    result1 <- getnewnode(data,class)
    result <- Node$new(colnames(data)[result1[[2]]], split = result1[[1]], samples = rownames(data), featurecolumn = result1[[2]])
    newclass1 <- t(class)[result1[[3]]]
    newdata1 <- data[result1[[3]],-result1[[2]]]
    newclass2 <- t(class)[result1[[4]]]
    newdata2 <- (data[result1[[4]],-result1[[1]]])
    #Add child nodes from the split and recursively continue
    result$AddChildNode(build.tree(newdata1, newclass1, nmin, minleaf, nfeat))
    result$AddChildNode(build.tree(newdata2, newclass2, nmin, minleaf, nfeat))
    return(result)
  }
  else{
    return(Node$new(paste("Leaf", paste(rownames(data), collapse=",")), split= "Leaf", samples= rownames(data)))
  }
}

##Debug code
print(build.tree(credit.dat[1:5],credit.dat[6], 1, 1, 1), "split", "samples", "featurecolumn")
