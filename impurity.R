library(data.tree)

##
# We use the gini-index as described. This is the R-translation within our structure.
# Given a node x, we determine the impurity with formula i(x) = p(0|x)p(1|x) (two-class Gini-index).
# This function does that by counting the 0 or 1 classes and calculating the frequency and then using the formula.
##
tree.impurity <- function(x){
  if(length(x) > 0){
    zero_frequency <- length(x[x == 0])/length(x)
    one_frequency <- length(x[x == 1])/length(x)
    return(zero_frequency * one_frequency)
  }
  else{
    return(0)
  }
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
getsplits <- function(x){
  if(length(unique(x))>1){
    m <- embed(unique(sort(x)),2)
    return(rowMeans(m))
  }
  else{
    return(unique(x))
  }
}

getnewnode <- function(x,y){
  result <- (apply(x,2,y=y,function(x,y) createCompMatrix(x,t(y))))
  bestsplitcolumn <- which.max(result[2,])
  bestsplit <-result[1,bestsplitcolumn]
  return(list(bestsplit, bestsplitcolumn, c(which(x[bestsplitcolumn] <= bestsplit)), c(which(x[bestsplitcolumn] > bestsplit))))
}


##
# Meat of the program.
# todo: elaborate
# build tree: to do: add parameters mentioned in assignment
##
build.tree <- function(x,y){
  if(tree.impurity(y)>0){
    result1 <- getnewnode(x,y)
    result <- Node$new(colnames(x)[result1[[2]]], split = result1[[1]], samples = rownames(x))
    newy1 <- t(y)[result1[[3]]]
    newx1 <- x[result1[[3]],-result1[[2]]]
    newy2 <- t(y)[result1[[4]]]
    newx2 <- (x[result1[[4]],-result1[[1]]])
    result$AddChildNode(build.tree(newx1, newy1))
    result$AddChildNode(build.tree(newx2, newy2))
    return(result)
  }
  else{
    return(Node$new(paste("Leaf", paste(rownames(x), collapse=",")), split= "Leaf", samples= rownames(x)))
  }
}

##Debug code
print(build.tree(credit.dat[1:5],credit.dat[6]), "split", "samples")
