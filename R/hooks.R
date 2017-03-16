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
