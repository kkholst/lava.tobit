'.onLoad' <- function(lib, pkg="lava.tobit") {
    lava::addhook("lava.tobit.estimate.hook","estimate.hooks")
    lava::addhook("lava.tobit.color.hook","color.hooks")
    lava::addhook("lava.tobit.sim.hook","sim.hooks")
    lava::addhook("lava.tobit.init.hook","init.hooks")

    lava::lava.options(tobitAlgorithm=mvtnorm::GenzBretz(abseps=1e-5),
                       tobitseed=1, threshold=1)
}

'.onAttach' <- function(lib, pkg="lava.tobit") {
    desc <- utils::packageDescription(pkg)
    packageStartupMessage(desc$Package, " version ",desc$Version)
}
