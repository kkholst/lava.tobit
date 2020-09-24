##'Plot distribution of standardized residuals
##'
##'Plot empirical (KM) and model-specific cumulative distribution function of
##'standardized residuals
##'
##'
##'@param x Model, \code{lvmfit} object
##'@param var Character vector of (endogenous) variable names
##'@param ylab Label of x-axis
##'@param xlab Label of y-axis
##'@param main Title of plot
##'@param k Optional group number for multiple group analysis
##'@param \dots Additional argument
##'@author Klaus K. Holst
##'@keywords models
##'@examples
##'
##'\dontrun{
##'
##' ## Simulate data where (y01,y2)
##' ## follows conditional bivariate normal distribution
##' ## given covariate x. Instead of y01 we observe
##' ## right censored version y2
##' n <- 200
##' m <- lvm(c(y01,y2) ~ x)
##' covariance(m) <- y01~y2
##' set.seed(1)
##' d <- sim(m,n)
##' d$cens1 <- rexp(n)
##' d$status1 <- with(d,y01<cens1)
##' d$y1 <- with(d, pmin(y01,cens1))
##'
##' ## Estimate model parameters
##' d$S1 <- with(d, Surv(y1,status1))
##' m <- lvm(c(S1,y2)~x); covariance(m) <- S1~y2
##' ## if (requireNamespace("mets", quietly=TRUE)) {
##' ##  e <- estimate(m,d,control=list(trace=1))##'
##' ##  ## Plot cumulative distribution functions
##' ##  par(mfrow=c(2,2)); plotres(e); plot(e)
##' ## }
##' }
##'
##' @export
plotres <- function(x,var=lava::endogenous(x),
                    ylab="Cumulative Distribution Function",
                    xlab="Standardized residuals",
                    main,
                    k,
            ...) {
    requireNamespace("survival")
    r <- stats::residuals(x,std=TRUE)
    W <- x$weights ## lava::Weights(x)
    W2 <- x$data2

    if (inherits(x,"multigroupfit")) {
        if (missing(k)) stop("Specify which group to assess.")
        r <- r[[k]]; W <- W[[k]]
    }

    for (v in var) {
        if (v %in% colnames(W2)) {
            S <- survival::Surv(r[,v],!is.infinite(W2[,v]))
        } else {
            if (v %in% colnames(W)) {
                S <- survival::Surv(ifelse(W[,v]==-1,NA,r[,v]),
                                   ifelse(W[,v]==1,NA,r[,v]),
                                   type="interval2")
            } else {
                S <- survival::Surv(r[,v],rep(TRUE,length(r[,v])))
            }
        }
        g <- survival::survfit(S~1)
        mymain <- ifelse(!missing(main),main,v)
        with(g,graphics::plot(1-surv~time,type="s",main=mymain,xlab=xlab,ylab=ylab))
        with(g,graphics::lines(1-upper~time,type="s",lty=2))
        with(g,graphics::lines(1-lower~time,type="s",lty=2))
        ro <- sort(r[,v]);
        graphics::lines(ro,stats::pnorm(ro),col="red",xlab=xlab,ylab=ylab)
    }
  invisible(x)
}
