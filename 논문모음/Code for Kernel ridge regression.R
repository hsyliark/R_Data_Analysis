#### Simulation data


dat.gen <- function(n, p, beta, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  x <- matrix( runif( n*p , -5 , 5 ), nrow=n , ncol=p )
  linpred <- colSums( rbind( rep(1, n), apply(x , 1 , function(tmp) tmp^(1:p)))*(beta) ) 
  y <- apply(as.matrix(linpred), c(1,2), rnorm, n=1, sd=0.2)
  dat <- cbind( y , x )
  colnames( dat ) <- c( "y" , paste0("x", 1:p) )
  return( as.data.frame(dat) )
}



#------------------------#------------------------#------------------------#------------------------




#### Making some functions




### 1. Making kernel matrix


my.kernel.matrix <- function(dat.train, dat.test) {
  
  # dat.train : Data frame for training with a response variable is appeared in the first column...
  # dat.test : Data frame for testing with a response variable is appeared in the first column...
  
  data1 <- as.data.frame(dat.train)
  data2 <- as.data.frame(dat.test)
  n <- nrow(data1)
  
  # Training data
  X.train <- as.matrix(data1[,-1])
  y.train <- as.matrix(data1[,1])
  
  # Test data
  X.test <- as.matrix(data2[,-1])
  y.test <- as.matrix(data2[,1])
  
  X1 <- rbind(X.train, X.test)
  
  
  p <- ncol(X1)
  sigma <- 1/p
  n1 <- nrow(X1)
  D <- as.matrix(dist(X1,method="euclidean",p=2)) # Euclidean distance 
  K <- exp( -sigma * D^2 )  # Gaussian kernel
  K.train <- K[1:n,1:n]
  K.test <- K[1:n,(n+1):n1]
  
  return(list(K.train=K.train, K.test=K.test, K=K, y.train=y.train, y.test=y.test))
  
}





### 2. Calculate coefficients using theory




my.kernel.regression <- function(y, K, lambda) {
  
  # y : response variable
  # K : matrix from Gaussian kernel transformation
  # lambda : penalty parameter
  
  if(lambda <= 0) 
    stop("Lambda is non-positive value. Please insert positive value of lambda.")
  
  y <- as.matrix(y) ; K <-as.matrix(K)
  
  d.hat <- solve(K + lambda*diag(x=1, nrow=length(y), ncol=length(y)))%*%y
  
  y.hat <- K%*%d.hat
  
  return(list(d.hat=d.hat, y.hat=y.hat))
  
}





### 3. Making function for fitting


fit.kernel <- function(y.train, K.train, lambda) {
  
  # y.train : Dependent variable of training data
  # K.train : Kernel matrix from training data 
  # lambda : penalty parameter
  
  if(lambda <= 0) 
    stop("Lambda is non-positive value. Please insert positive value of lambda.")
  
  
  y.train <- as.matrix(y.train)
  K.train <- as.matrix(K.train) 
  
  g <- my.kernel.regression(y.train, K.train, lambda)
  
  d.hat <- g$d.hat
  y.pred <- g$y.hat
  
  rmse <- sqrt(sum((y.train - y.pred)^2)/length(y.train))
  
  
  return(list(d.hat=d.hat, y.train=y.train, y.pred=y.pred, rmse=rmse))
  
}






### 4. Making function for predict


pred.kernel <- function(y.test, K.test, d.hat) {
  
  # y.test : Dependent variable of test data
  # K.test : Kernel matrix from test data 
  # d.hat : Estimator of vector d from training data
  
  
  y.test <- as.matrix(y.test) 
  K.test <- as.matrix(K.test)
  d.hat <- as.matrix(d.hat)
  
  
  y.hat <- t(K.test)%*%d.hat
  
  rmse <- sqrt(sum((y.test - y.hat)^2)/length(y.test))
  
  return(list(y.test=y.test, rmse=rmse, y.hat=y.hat))
  
}






### 5. Making function for K-fold crossvalidation


cv.kernel <- function(y.train, K.train, k, grid.l) {
  
  
  # y.train : Dependent variable of training data
  # K.train : Kernel matrix from training data 
  # k : number of criterion for K-fold crossvalidation
  # grid.l : The row of penalty parameter lambda
  
  check <- (grid.l > 0)
  n.check <- length(check)
  
  if(sum(check) != n.check)
    stop("Some of lambda's values are non-positive.
         Please insert positive values of lambda vector...","\n")
  
  
  lambda <- grid.l
  r <- length(lambda)
  
  
  K.sim <- as.matrix(K.train)
  y.sim <- as.matrix(y.train)
  n <- nrow(K.sim)
  
  cv.index <- sample(1:n,n,replace=F)  
  cv.rmse <- NULL   
  
  cat("K-fold crossvalidation is start...","\n")
  
  
  for (j in 1:r) {
    
    rmse <- NULL # Root mean squared error
    
    
    for (i in 0:(k-1)) {
      
      
      test.index <- cv.index[(1:n)%/%k==i]
      
      K.sim.train <- K.sim[-test.index, -test.index] ; K.sim.test <- K.sim[-test.index, test.index]
      y.sim.train <- y.sim[-test.index,] ; y.sim.test <- y.sim[test.index,]
      test.size <- length(test.index)
      
      
      a1 <- fit.kernel(y.sim.train, K.sim.train, lambda[j])
      train.d.hat <- a1$d.hat
      
      a2 <- pred.kernel(y.sim.test, K.sim.test, train.d.hat)
      test.y.hat <- a2$y.hat      
      
      
      rmse <- c(rmse, sqrt(sum((y.sim.test - test.y.hat)^2)/length(y.sim.test)) )
      
      
    }
    
    cv.rmse <- rbind(cv.rmse, rmse)
    cat(j, ",")
  }
  
  cat("\n","K-fold crossvalidation complete...")
  
  
  return(list(lambda=grid.l, cv.rmse=cv.rmse))
  
  
}





#------------------------#------------------------#------------------------#------------------------
#------------------------#------------------------#------------------------#------------------------




#### 8 method simulation


### Function for 1 independent variable

fit.ftn1 <- function(n.train, p, beta) {
  
  
    
  ### 1. Kernel ridge regression (KR)
  
  KR <- c(rep(0,100))
  
  for (i in 1:100) {
    
    # Making simulation data
    
    n.train <- n.train
    p <- p
    beta <- beta
    
    train.sim <- dat.gen(n.train, p, beta, seed=i)
    test.sim <- dat.gen(1000, p, beta, seed=i)
    
    
    # 5-fold crossvalidation
    
    u <- my.kernel.matrix(train.sim, test.sim)
    K.train <- u$K.train ; K.test <- u$K.test  
    y.train <- u$y.train ; y.test <- u$y.test
    
    grid.l <- 10^seq(-3,2,length=10)
    
    h <- cv.kernel(y.train, K.train, 5, grid.l) 
    
    # Fitting 
    
    mean.rmse <- rowMeans(h$cv.rmse)
    idx <- which.min(mean.rmse)
    best.lam <- max(h$lambda[ mean.rmse == mean.rmse[idx] ])
    
    h1 <- fit.kernel(y.train, K.train, best.lam)
    
    # Calculate test RMSE
    
    sim.d.hat <- h1$d.hat
    
    h2 <- pred.kernel(y.test, K.test, sim.d.hat)
    KR[i] <- h2$rmse
    
  }
  
  KR
  boxplot(KR)
  dat1 <- data.frame(RMSE=KR, method=rep("KR",100), number=as.character(rep(n.train,100)))
  
  
   
  
  ### 2. Kernel ridge regression using Sub-sampling (KRS)
  
  KRS <- c(rep(0,100))
  
  for (i in 1:100) {
    
    # Making simulation data
    
    n.train <- n.train
    p <- p
    beta <- beta
    
    train.sim <- dat.gen(n.train, p, beta, seed=i)
    test.sim <- dat.gen(1000, p, beta, seed=i)
    
    u <- my.kernel.matrix(train.sim, test.sim)
    K.train <- u$K.train ; y.train <- u$y.train 
    K <- u$K ; y.test <- u$y.test
    
    # Choosing optimal lambda
    
    
    n <- nrow(train.sim) ; n1 <- sum(nrow(train.sim), nrow(test.sim))
    res.rmse.c <- NULL ; res.lam.c <- NULL ; res.index.c <- NULL
    
    for (j in 1:50) {
      
      n <- nrow(train.sim)
      index.Kc <- sample(1:n, round(0.7*n), replace=F)
      Kc.train <- K.train[index.Kc,index.Kc] ; Kc.test <- K.train[index.Kc,-index.Kc]
      yc.train <- y.train[index.Kc] ; yc.test <- y.train[-index.Kc]
      
      grid.l <- 10^seq(-3,2,length=10)
      
      hc <- cv.kernel(yc.train, Kc.train, 5, grid.l) # 5-fold crossvalidation 
      
      mean.rmse.c <- rowMeans(hc$cv.rmse)
      idx.c <- which.min(mean.rmse.c)
      best.lam.c <- max(hc$lambda[ mean.rmse.c == mean.rmse.c[idx.c] ])
      
      h1.c <- fit.kernel(yc.train, Kc.train, best.lam.c) # Fitting
      
      sim.d.hat.c <- h1.c$d.hat
      
      h2.c <- pred.kernel(yc.test, Kc.test, sim.d.hat.c) # Testing
      rmse.c <- h2.c$rmse
      
      res.rmse.c <- c(res.rmse.c, rmse.c)
      res.lam.c <- c(res.lam.c, best.lam.c)
      res.index.c <- cbind(res.index.c, index.Kc)
      
    }
    
    # Fitting  
    
    res.lambda <- res.lam.c[which.min(res.rmse.c)]
    res.index <- res.index.c[, which.min(res.rmse.c)]
    res.K.train <- K[res.index, res.index]
    res.y.train <- y.train[res.index]
    K.test <- K[res.index, (n+1):n1]
    
    h1 <- fit.kernel(res.y.train, res.K.train, res.lambda)
    
    res.d.hat <- h1$d.hat
    
    # Calculate test mean square error
    
    h2 <- pred.kernel(y.test, K.test, res.d.hat)
    KRS[i] <- h2$rmse
    
  }
  
  KRS
  boxplot(KRS)
  dat2 <- data.frame(RMSE=KRS, method=rep("KRS",100), number=as.character(rep(n.train,100)))
  
  
  
  
  ### 3. Kernel ridge regression using Bagging (KRB)
  
  KRB <- c(rep(0,100))
  
  for (s in 1:100) {
    
    # Making simulation data
    
    n.train <- n.train
    p <- p
    beta <- beta
    
    train.sim <- dat.gen(n.train, p, beta, seed=s)
    test.sim <- dat.gen(1000, p, beta, seed=s)
    test.sim.y <- test.sim[,1] 
    
    u <- my.kernel.matrix(train.sim, test.sim)
    K.train <- u$K.train 
    y.train <- u$y.train ; y.test <- u$y.test 
    K <- u$K
    n <- nrow(train.sim)
    n1 <- sum(nrow(train.sim), nrow(test.sim))
    
    
    # Choosing best lambda
    
    res.rmse <- NULL
    grid.l <- 10^seq(-3,2,length=10)
    
    for (i in 1:50) {
      
      boot.index <- sample(1:n, n, replace=T)
      boot.K.train <- K.train[boot.index, boot.index]
      boot.K.test <- K.train[boot.index, -boot.index]
      boot.y.train <- y.train[boot.index]
      boot.y.test <- y.train[-boot.index]
      
      boot.rmse <- c(rep(0,10))
      
      for (j in 1:10) {
        
        h1.boot <- fit.kernel(boot.y.train, boot.K.train, grid.l[j])
        boot.d.hat <- h1.boot$d.hat
        h2.boot <- pred.kernel(boot.y.test, boot.K.test, boot.d.hat)
        boot.rmse[j] <- h2.boot$rmse
        
      }
      
      res.rmse <- rbind(res.rmse, boot.rmse)
      
    }
    
    #best.lambda <- grid.l[which.min(colMeans(res.rate.miss))]
    best.lambda <- max(grid.l[colMeans(res.rmse) == min(colMeans(res.rmse))])
    
    # Bootstraping
    
    boots.y <- NULL
    
    for (r in 1:100) {
      
      boots.index <- sample(1:n, n, replace=T)
      boots.K.train <- K[boots.index, boots.index]
      boots.K.test <- K[boots.index, (n+1):n1]
      boots.y.train <- y.train[boots.index]
      boots.y.test <- y.test 
      
      h1.boots <- fit.kernel(boots.y.train, boots.K.train, best.lambda)
      boots.d.hat <- h1.boots$d.hat
      h2.boots <- pred.kernel(boots.y.test, boots.K.test, boots.d.hat)
      
      boots.y.hat <- h2.boots$y.hat
      boots.y <- cbind(boots.y, boots.y.hat)
      
    }
    cat(s, ",")
    
    # Bagging estimate 
    
    y.hat.bag <- rowMeans(boots.y)
    
    KRB[s] <- sqrt(sum((y.test - y.hat.bag)^2)/length(y.test))
    
    
  }
  
  KRB
  boxplot(KRB)
  dat3 <- data.frame(RMSE=KRB, method=rep("KRB",100), number=as.character(rep(n.train,100)))
  

  
    
  
  ### 4. Kernel ridge regression using Random Forest (KRR)
  
  KRR <- c(rep(0,100))
  
  for (s in 1:100) {
    
    # Making simulation data
    
    n.train <- n.train
    p <- p
    beta <- beta
    
    train.sim <- dat.gen(n.train, p, beta, seed=s)
    test.sim <- dat.gen(1000, p, beta, seed=s)
    train.sim.y <- train.sim[,1] ; test.sim.y <- test.sim[,1]
    train.sim.X <- train.sim[,-1] ; test.sim.X <- test.sim[,-1]
    
    n <- nrow(train.sim)
    n1 <- sum(nrow(train.sim), nrow(test.sim))
    
    
    # Choosing best lambda
    
    res.rmse <- NULL
    grid.l <- 10^seq(-3,2,length=10)
    
    for (i in 1:50) {
      
      rf.index <- sample(1:p, round(sqrt(p)), replace=F)
      boot.index <- sample(1:n, n, replace=T)
      train.sim1 <- cbind(train.sim.y[boot.index], train.sim.X[boot.index])
      test.sim1 <- cbind(train.sim.y[-boot.index], train.sim.X[-boot.index])
      
      u1 <- my.kernel.matrix(train.sim1, test.sim1)
      boot.K.train <- u1$K.train ; boot.K.test <- u1$K.test
      boot.y.train <- u1$y.train ; boot.y.test <- u1$y.test
      
      boot.rmse <- c(rep(0,10))
      
      for (j in 1:10) {
        
        h1.boot <- fit.kernel(boot.y.train, boot.K.train, grid.l[j])
        boot.d.hat <- h1.boot$d.hat
        h2.boot <- pred.kernel(boot.y.test, boot.K.test, boot.d.hat)
        boot.rmse[j] <- h2.boot$rmse
        
      }
      
      res.rmse <- rbind(res.rmse, boot.rmse)
      
    }
    
    #best.lambda <- grid.l[which.min(colMeans(res.rate.miss))]
    best.lambda <- max(grid.l[colMeans(res.rmse)==min(colMeans(res.rmse))])
    
    # Bootstraping
    
    boots.y <- NULL
    
    for (r in 1:100) {
      
      rfs.index <- sample(1:p, round(sqrt(p)), replace=F)
      boots.index <- sample(1:n, n, replace=T)
      train.sim2 <- cbind(train.sim.y[boots.index], train.sim.X[boots.index])
      test.sim2 <- cbind(test.sim.y, test.sim.X)
      
      u2 <- my.kernel.matrix(train.sim2, test.sim2)
      boots.K.train <- u2$K.train ; boots.K.test <- u2$K.test
      boots.y.train <- u2$y.train ; boots.y.test <- u2$y.test
      
      h1.boots <- fit.kernel(boots.y.train, boots.K.train, best.lambda)
      boots.d.hat <- h1.boots$d.hat
      h2.boots <- pred.kernel(boots.y.test, boots.K.test, boots.d.hat)
      
      boots.y.hat <- h2.boots$y.hat
      boots.y <- cbind(boots.y, boots.y.hat)
      
    }
    cat(s, ",")
    
    # Random Forest estimate 
    
    y.hat.rf <- rowMeans(boots.y)
    
    KRR[s] <- sqrt(sum((test.sim.y - y.hat.rf)^2)/length(test.sim.y))
    
  }
  
  KRR
  boxplot(KRR)
  dat4 <- data.frame(RMSE=KRR, method=rep("KRR",100), number=as.character(rep(n.train,100)))
  
  
  
  # Final result
  
  dat.res <- rbind(dat1, dat2, dat3, dat4)
  
  return(dat.res)
  
}



### Function for many independent variables



fit.ftn <- function(n.train, p, beta) {
  
  
  
  
  ### 1. Kernel ridge regression (KR)
  
  KR <- c(rep(0,100))
  
  for (i in 1:100) {
    
    # Making simulation data
    
    n.train <- n.train
    p <- p
    beta <- beta
    
    train.sim <- dat.gen(n.train, p, beta, seed=i)
    test.sim <- dat.gen(1000, p, beta, seed=i)
    
    
    # 5-fold crossvalidation
    
    u <- my.kernel.matrix(train.sim, test.sim)
    K.train <- u$K.train ; K.test <- u$K.test  
    y.train <- u$y.train ; y.test <- u$y.test
    
    grid.l <- 10^seq(-3,2,length=10)
    
    h <- cv.kernel(y.train, K.train, 5, grid.l) 
    
    # Fitting 
    
    mean.rmse <- rowMeans(h$cv.rmse)
    idx <- which.min(mean.rmse)
    best.lam <- max(h$lambda[ mean.rmse == mean.rmse[idx] ])
    
    h1 <- fit.kernel(y.train, K.train, best.lam)
    
    # Calculate test RMSE
    
    sim.d.hat <- h1$d.hat
    
    h2 <- pred.kernel(y.test, K.test, sim.d.hat)
    KR[i] <- h2$rmse
    
  }
  
  KR
  boxplot(KR)
  dat1 <- data.frame(RMSE=KR, method=rep("KR",100), number=as.character(rep(n.train,100)))
  
  
  
  
  
  ### 2. Kernel ridge regression using Sub-sampling (KRS)
  
  KRS <- c(rep(0,100))
  
  for (i in 1:100) {
    
    # Making simulation data
    
    n.train <- n.train
    p <- p
    beta <- beta
    
    train.sim <- dat.gen(n.train, p, beta, seed=i)
    test.sim <- dat.gen(1000, p, beta, seed=i)
    
    u <- my.kernel.matrix(train.sim, test.sim)
    K.train <- u$K.train ; y.train <- u$y.train 
    K <- u$K ; y.test <- u$y.test
    
    # Choosing optimal lambda
    
    
    n <- nrow(train.sim) ; n1 <- sum(nrow(train.sim), nrow(test.sim))
    res.rmse.c <- NULL ; res.lam.c <- NULL ; res.index.c <- NULL
    
    for (j in 1:50) {
      
      n <- nrow(train.sim)
      index.Kc <- sample(1:n, round(0.7*n), replace=F)
      Kc.train <- K.train[index.Kc,index.Kc] ; Kc.test <- K.train[index.Kc,-index.Kc]
      yc.train <- y.train[index.Kc] ; yc.test <- y.train[-index.Kc]
      
      grid.l <- 10^seq(-3,2,length=10)
      
      hc <- cv.kernel(yc.train, Kc.train, 5, grid.l) # 5-fold crossvalidation 
      
      mean.rmse.c <- rowMeans(hc$cv.rmse)
      idx.c <- which.min(mean.rmse.c)
      best.lam.c <- max(hc$lambda[ mean.rmse.c == mean.rmse.c[idx.c] ])
      
      h1.c <- fit.kernel(yc.train, Kc.train, best.lam.c) # Fitting
      
      sim.d.hat.c <- h1.c$d.hat
      
      h2.c <- pred.kernel(yc.test, Kc.test, sim.d.hat.c) # Testing
      rmse.c <- h2.c$rmse
      
      res.rmse.c <- c(res.rmse.c, rmse.c)
      res.lam.c <- c(res.lam.c, best.lam.c)
      res.index.c <- cbind(res.index.c, index.Kc)
      
    }
    
    # Fitting  
    
    res.lambda <- res.lam.c[which.min(res.rmse.c)]
    res.index <- res.index.c[, which.min(res.rmse.c)]
    res.K.train <- K[res.index, res.index]
    res.y.train <- y.train[res.index]
    K.test <- K[res.index, (n+1):n1]
    
    h1 <- fit.kernel(res.y.train, res.K.train, res.lambda)
    
    res.d.hat <- h1$d.hat
    
    # Calculate test mean square error
    
    h2 <- pred.kernel(y.test, K.test, res.d.hat)
    KRS[i] <- h2$rmse
    
  }
  
  KRS
  boxplot(KRS)
  dat2 <- data.frame(RMSE=KRS, method=rep("KRS",100), number=as.character(rep(n.train,100)))
  
  
  
  
  
  ### 3. Kernel ridge regression using Bagging (KRB)
  
  KRB <- c(rep(0,100))
  
  for (s in 1:100) {
    
    # Making simulation data
    
    n.train <- n.train
    p <- p
    beta <- beta
    
    train.sim <- dat.gen(n.train, p, beta, seed=s)
    test.sim <- dat.gen(1000, p, beta, seed=s)
    test.sim.y <- test.sim[,1] 
    
    u <- my.kernel.matrix(train.sim, test.sim)
    K.train <- u$K.train 
    y.train <- u$y.train ; y.test <- u$y.test 
    K <- u$K
    n <- nrow(train.sim)
    n1 <- sum(nrow(train.sim), nrow(test.sim))
    
    
    # Choosing best lambda
    
    res.rmse <- NULL
    grid.l <- 10^seq(-3,2,length=10)
    
    for (i in 1:50) {
      
      boot.index <- sample(1:n, n, replace=T)
      boot.K.train <- K.train[boot.index, boot.index]
      boot.K.test <- K.train[boot.index, -boot.index]
      boot.y.train <- y.train[boot.index]
      boot.y.test <- y.train[-boot.index]
      
      boot.rmse <- c(rep(0,10))
      
      for (j in 1:10) {
        
        h1.boot <- fit.kernel(boot.y.train, boot.K.train, grid.l[j])
        boot.d.hat <- h1.boot$d.hat
        h2.boot <- pred.kernel(boot.y.test, boot.K.test, boot.d.hat)
        boot.rmse[j] <- h2.boot$rmse
        
      }
      
      res.rmse <- rbind(res.rmse, boot.rmse)
      
    }
    
    #best.lambda <- grid.l[which.min(colMeans(res.rate.miss))]
    best.lambda <- max(grid.l[colMeans(res.rmse) == min(colMeans(res.rmse))])
    
    # Bootstraping
    
    boots.y <- NULL
    
    for (r in 1:100) {
      
      boots.index <- sample(1:n, n, replace=T)
      boots.K.train <- K[boots.index, boots.index]
      boots.K.test <- K[boots.index, (n+1):n1]
      boots.y.train <- y.train[boots.index]
      boots.y.test <- y.test 
      
      h1.boots <- fit.kernel(boots.y.train, boots.K.train, best.lambda)
      boots.d.hat <- h1.boots$d.hat
      h2.boots <- pred.kernel(boots.y.test, boots.K.test, boots.d.hat)
      
      boots.y.hat <- h2.boots$y.hat
      boots.y <- cbind(boots.y, boots.y.hat)
      
    }
    cat(s, ",")
    
    # Bagging estimate 
    
    y.hat.bag <- rowMeans(boots.y)
    
    KRB[s] <- sqrt(sum((y.test - y.hat.bag)^2)/length(y.test))
    
    
  }
  
  KRB
  boxplot(KRB)
  dat3 <- data.frame(RMSE=KRB, method=rep("KRB",100), number=as.character(rep(n.train,100)))
  
  
  
  
  
  
  ### 4. Kernel ridge regression using Random Forest (KRR)
  
  KRR <- c(rep(0,100))
  
  for (s in 1:100) {
    
    # Making simulation data
    
    n.train <- n.train
    p <- p
    beta <- beta
    
    train.sim <- dat.gen(n.train, p, beta, seed=s)
    test.sim <- dat.gen(1000, p, beta, seed=s)
    train.sim.y <- train.sim[,1] ; test.sim.y <- test.sim[,1]
    train.sim.X <- train.sim[,-1] ; test.sim.X <- test.sim[,-1]
    
    n <- nrow(train.sim)
    n1 <- sum(nrow(train.sim), nrow(test.sim))
    
    
    # Choosing best lambda
    
    res.rmse <- NULL
    grid.l <- 10^seq(-3,2,length=10)
    
    for (i in 1:50) {
      
      rf.index <- sample(1:p, round(sqrt(p)), replace=F)
      boot.index <- sample(1:n, n, replace=T)
      train.sim1 <- cbind(train.sim.y[boot.index], train.sim.X[boot.index, rf.index])
      test.sim1 <- cbind(train.sim.y[-boot.index], train.sim.X[-boot.index, rf.index])
      
      u1 <- my.kernel.matrix(train.sim1, test.sim1)
      boot.K.train <- u1$K.train ; boot.K.test <- u1$K.test
      boot.y.train <- u1$y.train ; boot.y.test <- u1$y.test
      
      boot.rmse <- c(rep(0,10))
      
      for (j in 1:10) {
        
        h1.boot <- fit.kernel(boot.y.train, boot.K.train, grid.l[j])
        boot.d.hat <- h1.boot$d.hat
        h2.boot <- pred.kernel(boot.y.test, boot.K.test, boot.d.hat)
        boot.rmse[j] <- h2.boot$rmse
        
      }
      
      res.rmse <- rbind(res.rmse, boot.rmse)
      
    }
    
    #best.lambda <- grid.l[which.min(colMeans(res.rate.miss))]
    best.lambda <- max(grid.l[colMeans(res.rmse)==min(colMeans(res.rmse))])
    
    # Bootstraping
    
    boots.y <- NULL
    
    for (r in 1:100) {
      
      rfs.index <- sample(1:p, round(sqrt(p)), replace=F)
      boots.index <- sample(1:n, n, replace=T)
      train.sim2 <- cbind(train.sim.y[boots.index], train.sim.X[boots.index, rfs.index])
      test.sim2 <- cbind(test.sim.y, test.sim.X[, rfs.index])
      
      u2 <- my.kernel.matrix(train.sim2, test.sim2)
      boots.K.train <- u2$K.train ; boots.K.test <- u2$K.test
      boots.y.train <- u2$y.train ; boots.y.test <- u2$y.test
      
      h1.boots <- fit.kernel(boots.y.train, boots.K.train, best.lambda)
      boots.d.hat <- h1.boots$d.hat
      h2.boots <- pred.kernel(boots.y.test, boots.K.test, boots.d.hat)
      
      boots.y.hat <- h2.boots$y.hat
      boots.y <- cbind(boots.y, boots.y.hat)
      
    }
    cat(s, ",")
    
    # Random Forest estimate 
    
    y.hat.rf <- rowMeans(boots.y)
    
    KRR[s] <- sqrt(sum((test.sim.y - y.hat.rf)^2)/length(test.sim.y))
    
  }
  
  KRR
  boxplot(KRR)
  dat4 <- data.frame(RMSE=KRR, method=rep("KRR",100), number=as.character(rep(n.train,100)))
  
  
  
  # Final result
  
  dat.res <- rbind(dat1, dat2, dat3, dat4)
  
  return(dat.res)
  
}


install.packages("ggplot2")
library(ggplot2)





### Final result (p=1)

dat1.1 <- fit.ftn1(50, 1, 0.2^(0:1)) # Choose when n.train=50
dat1.2 <- fit.ftn1(100, 1, 0.2^(0:1)) # Choose when n.train=100
dat1.3 <- fit.ftn1(200, 1, 0.2^(0:1)) # Choose when n.train=200
dat1.4 <- fit.ftn1(400, 1, 0.2^(0:1)) # Choose when n.train=400

final.dat1 <- rbind(dat1.1, dat1.2, dat1.3, dat1.4)

ggplot(final.dat1, aes(x = number, y = RMSE, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=4) 





### Final result (p=2)

dat2.1 <- fit.ftn(50, 2, 0.2^(0:2)) # Choose when n.train=50
dat2.2 <- fit.ftn(100, 2, 0.2^(0:2)) # Choose when n.train=100
dat2.3 <- fit.ftn(200, 2, 0.2^(0:2)) # Choose when n.train=200
dat2.4 <- fit.ftn(400, 2, 0.2^(0:2)) # Choose when n.train=400

final.dat2 <- rbind(dat2.1, dat2.2, dat2.3, dat2.4)

ggplot(final.dat2, aes(x = number, y = RMSE, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=4)





### Final result (p=5)

dat3.1 <- fit.ftn(50, 5, 0.2^(0:5)) # Choose when n.train=50
dat3.2 <- fit.ftn(100, 5, 0.2^(0:5)) # Choose when n.train=100
dat3.3 <- fit.ftn(200, 5, 0.2^(0:5)) # Choose when n.train=200
dat3.4 <- fit.ftn(400, 5, 0.2^(0:5)) # Choose when n.train=400

final.dat5 <- rbind(dat3.1, dat3.2, dat3.3, dat3.4)

ggplot(final.dat5, aes(x = number, y = RMSE, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=4)





### Final result (p=10)

dat4.1 <- fit.ftn(50, 10, 0.2^(0:10)) # Choose when n.train=50
dat4.2 <- fit.ftn(100, 10, 0.2^(0:10)) # Choose when n.train=100
dat4.3 <- fit.ftn(200, 10, 0.2^(0:10)) # Choose when n.train=200
dat4.4 <- fit.ftn(400, 10, 0.2^(0:10)) # Choose when n.train=400

final.dat10 <- rbind(dat4.1, dat4.2, dat4.3, dat4.4)

ggplot(final.dat10, aes(x = number, y = RMSE, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=4)





### Final result (p=20)

dat5.1 <- fit.ftn(50, 20, 0.2^(0:20)) # Choose when n.train=50
dat5.2 <- fit.ftn(100, 20, 0.2^(0:20)) # Choose when n.train=100
dat5.3 <- fit.ftn(200, 20, 0.2^(0:20)) # Choose when n.train=200
dat5.4 <- fit.ftn(400, 20, 0.2^(0:20)) # Choose when n.train=400

final.dat20 <- rbind(dat5.1, dat5.2, dat5.3, dat5.4)

ggplot(final.dat20, aes(x = number, y = RMSE, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=4)





### Final result (p=40)

dat6.1 <- fit.ftn(50, 40, 0.2^(0:40)) # Choose when n.train=50
dat6.2 <- fit.ftn(100, 40, 0.2^(0:40)) # Choose when n.train=100
dat6.3 <- fit.ftn(200, 40, 0.2^(0:40)) # Choose when n.train=200
dat6.4 <- fit.ftn(400, 40, 0.2^(0:40)) # Choose when n.train=400

final.dat40 <- rbind(dat6.1, dat6.2, dat6.3, dat6.4)

ggplot(final.dat40, aes(x = number, y = RMSE, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=4)

