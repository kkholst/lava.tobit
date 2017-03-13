##' @export
lava.tobit.init.hook <- function(x,...) {
  x$attributes$binary <- list()
  return(x)
  ##  nodeDataDefaults(x,"binary") <- FALSE; x
}

##' @export
lava.tobit.sim.hook <- function(x,data,...) {  
  if (length(binary(x))>0)
    data[,binary(x)] <- (data[,binary(x)]>0)*1
  return(data)
}

##' @export
lava.tobit.color.hook <- function(x,subset=lava::vars(x),...) {
  return(list(vars=intersect(subset,binary(x)),col="indianred1"))
}

##' @export
lava.tobit.estimate.hook <- function(x,data,weights,data2,estimator,...) {
  dots <- list(...)
## Binary outcomes -> censored regression
  bin <- intersect(binary(x),lava::vars(x))
  if (is.null(dim(data))) return(NULL)
  if (is.null(estimator) || estimator%in%c("gaussian","tobit","normal")) {
    for (i in setdiff(lava::endogenous(x),binary(x))) {
      if (is.character(data[,i]) | is.factor(data[,i])) { # Transform binary 'factor'
        y <- as.factor(data[,i])
        if (nlevels(y)==2) {
          data[,i] <- as.numeric(y)-1
          binary(x) <- i
        }
      }
    }
    if (length(bin)>0) {
      estimator <- "tobit"
      if (is.null(weights)) {        
        W <- data[,bin,drop=FALSE]; W[W==0] <- -1; colnames(W) <- bin
        weights <- lava::lava.options()$threshold*W
      } else {
        ##        if (!all(binary(x)%in%colnames(data)))
        ##        W <- data[,binary(x),drop=FALSE]; W[W==0] <- -1; colnames(W) <- binary(x)
        ##        attributes(W)$data2 <- weights
        ##        weights <- W
        ##          weights[,binary(x)] <- W
      }
      for (b in bin) {
        data[!is.na(data[,b]),b] <- 0
      }
      ##    data[,binary(x)] <- 0
      if (!is.null(data2)) {
        estimator <- "tobitw"
      }
    }
  }
  
  ## Transform 'Surv' objects
  data2 <- mynames <- NULL
  if (is.null(estimator) || estimator%in%c("normal")) {
    for (i in setdiff(lava::endogenous(x),bin)) {
      if (survival::is.Surv(data[,i])) { 
        S <- data[,i]
        y1 <- S[,1]
        if (attributes(S)$type=="left")  {
          y2 <- y1
          y1[S[,2]==0] <- -Inf          
        }
        if (attributes(S)$type=="right") {
          y2 <- y1
          y2[S[,2]==0] <- Inf
        }
        if (attributes(S)$type=="interval2") {
          y2 <- S[,2]
        }
        if (attributes(S)$type=="interval") {
            y2 <- S[,2]
            y2[S[,3]==1L] <- y1[S[,3]==1L]
        }
        if (!(attributes(S)$type%in%c("left","right","interval2","interval"))) stop("Surv type not supported.")
        mynames <- c(mynames,i)
        y2 <- cbind(y2)
        colnames(y2) <- i
        data2 <- cbind(data2,y2)
        data[,i] <- y1
        estimator <- "normal"
      }
    }
  }

  W <- NULL
  if (length(estimator)>0 && estimator%in%c("gaussian","tobit","tobitw")) {
    for (i in setdiff(lava::endogenous(x),bin)) {
      if (survival::is.Surv(data[,i])) { 
        estimator <- "tobit"
        S <- data[,i]
        y <- S[,1]
        if (attributes(S)$type=="left") 
          w <- S[,2]-1
        if (attributes(S)$type=="right") 
          w <- 1-S[,2]
        if (attributes(S)$type=="interval2") {
          w <- S[,3]; w[w==2] <- (-1)
        }
        mynames <- c(mynames,i)
        W <- cbind(W,w)
        data[,i] <- y
      }
    }
    if (length(W)>0) {
      colnames(W) <- mynames
      if (!is.null(weights)) {
        wW <- intersect(colnames(weights),colnames(W))
        if (length(wW)>0)
          weights[,wW] <- W[,wW]
        Wo <- setdiff(colnames(W),wW)
        if (length(Wo)>0)
        weights <- cbind(weights,W[,Wo,drop=FALSE])
      } else {
        weights <- W;
      }
    }
  }
  return(c(list(x=x,data=data,weights=weights,data2=data2,estimator=estimator),dots)) 
}
