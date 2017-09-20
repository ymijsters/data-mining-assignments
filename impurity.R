library(data.tree)
impurity <- function(x){
  partition0 <- length(x[x == 0])/length(x)
  partition1 <- length(x[x == 1])/length(x)
  return(partition0 * partition1)
}

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
#Rloops should be avoided
calculate.best.split <- function(x,y){
  currentImp = impurity(t(y))
  splitlist <- vector()
  split <- t(unique(x))
  if(length(x[x<=split]) > 0&& length(x[x>split])>0){
    splitlist <- c(splitlist, redistribution(x,y,split,currentImp))
  }
  print(max(t(max(splitlist))))
}

redistribution <- function(x,y){
  currentImp <- impurity(y)
  redis <- currentImp - (length(x[x==0])/length(x))*impurity(y[which(x==0)]) - 
    (length(x[x==1])/length(x))*impurity(y[which(x==1)])
  return(redis)
}
#tree.grow(credit.dat[1:5],credit.dat[6],0,0,0)
createCompMatrix <- function(x,y){
  compmatrixx1 <- expand.grid(x,getsplits(x))
  splitmatrix <- matrix(as.numeric(compmatrixx1[,1] <= compmatrixx1[,2]), nrow=length(x))
  impurity <- apply(splitmatrix,2, y=y, function(splitmatrix,y) redistribution(splitmatrix,y))
  return(c(getsplits(x)[which.max(impurity)],max(impurity)))
}
getsplits <- function(x){
  if(length(unique(x))>1){
    m <- embed(unique(sort(x)),2)
    return(rowMeans(m))
  }
  else{
    return(unique(x))
  }
}

build.tree <- function(x,y,result){
  if(impurity(y)>0){
    result1 <- (apply(x,2,y=y,function(x,y) createCompMatrix(x,t(y))))
    print(result1)
    result<-c(result, (result1[which.max(result1[2,])]))
    rowschild1 = which(x[which.max(result1[2,])]<=result1[1,which.max(result1[2,])])
    uselesscolumns = c(which.max(result1[2,]), which(sapply(x, function(x) length(unique(x))<=1)==1))
    newy1 <- t(y)[rowschild1]
    newx1 <- (x[rowschild1,-uselesscolumns])
    print(sapply(newx1, function(x) print(length(unique(x)))))
    build.tree(newx1, newy1, result)
    newy2 <- t(y)[-rowschild1]
    newx2 <- (x[-rowschild1,-uselesscolumns])
    print(sapply(newx2, function(x) print(length(unique(x)))))
    build.tree(newx2, newy2,result)
  }
  else{
    return(result)
  }
}
print(build.tree(credit.dat[1:5],credit.dat[6], c()))

# <-trainingset[-(which.max(result1[2,])),which(trainingset[which.max(result1[2,])]<=result1[1,which.max(result1[2,])])]
#rowschild1 = which(trainingset[which.max(result1[2,])]<=result1[1,which.max(result1[2,])])
#newtrainingset1 <- (trainingset[rowschild1,-which.max(result1[2,])])
#result1child1 <- apply(newtrainingset1, 2,y=credit.dat[rowschild1,6],function(x,y) createCompMatrix(x,y))
#print(result1child1)
#rowschild2 = which(newtrainingset1[which.max(result1child1[2,])]>result1child1[1,which.max(result1child1[2,])])
#newtrainingset2 <- (trainingset[rowschild2,-which.max(result1[2,])])
#result1child1child1 <- apply(newtrainingset2, 2,y=credit.dat[rowschild2,6],function(x,y) createCompMatrix(x,y))
#print(result1child1child1)