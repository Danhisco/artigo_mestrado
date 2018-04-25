library(rmutil)

#' Kernel quantile
#'
#' Returns the quantile value that acumulates a given percentile of
#' the density of three types of dispersion kernels in a simulation of
#' a spatially explicit neutral dynamics (Rosindell et al. 2008).
#'
#' @param sigma real positive, the dispersion parameter of the kernel. For
#'     uniform it is the total width of the kernel (max - min), and for
#'     gaussian and laplacian kernels sigma is the standard deviation.
#' @param kernel character, either "normal", "uniform" or "laplacian";
#'     the kernel type. See Rosindell et al. (2008) for details.
#' @param p real 0 < p > 1; the accumulated probability from the kernel
#'     center.
#' @param density real positive, the density of individual in the
#'     simulation grid, in individuals/hectare
#' @return kernel quantile, which is the distance in meters from the kernel
#'     center that encompasses a proportion p of total kernel
#'     density. For example, p = 0.5 returns the median dispersal
#'     distance.
#' @param npoints non-negative integer, number of distance points to
#'     simulate.
#' @details This function simulates the sampling of dispersion
#'     distances from the kernel as outlined by Rosindell et al
#'     (2008), which is not the same as a bivariate version of each
#'     distribution (e.g. a bivariate normal, laplacian,
#'     uniform). Rather, X and Y coordinates are sampled independently
#'     and the used to find the dispersal distance. Though an
#'     analytical solution might be feasible, this function uses the
#'     random sample functions of the univariate distributions and
#'     empirical quantile functions and so provides approximate values.
#' @references Rosindell J, Wong Y, Etienne RS, 2008, A coalescence
#' approach to spatial neutral ecology, Ecological Informatics, Vol: 3,
#' Pages: 259-271 
qkernel<- function(sigma, kernel, p, density=20852/50, npoints = 1e5){
    kernel <- match.arg(kernel, choices=c("normal","gaussian","laplace","uniform"))
    ##metro na escala da simulacao
    d_ind_MA  <- 100/sqrt(density) #100/sqrt(DA)
    if(kernel=="laplace"){
        b_laplace <- sigma / sqrt(2) ## relação entre sigma e b, a variância da laplaciana
        ## gerando valores de uma distribuição laplaciana e convertendo para a distância em metros
        X_laplace <- d_ind_MA * round(rlaplace(npoints, s=b_laplace) / d_ind_MA) 
        Y_laplace <- d_ind_MA * round(rlaplace(npoints, s=b_laplace) / d_ind_MA) # idem para Y
        dist_laplace <- sqrt(X_laplace^2+Y_laplace^2) #distâncias dos pontos até a origem
        result <- quantile(dist_laplace, p) #guardando
    }
    if(kernel=="normal"|kernel=="gaussian"){
        b_norm <- sigma 
        X_norm <- d_ind_MA * round(rnorm(npoints, sd=b_norm) / d_ind_MA)
        Y_norm <- d_ind_MA * round(rnorm(npoints, sd=b_norm) / d_ind_MA)
        dist_norm <- sqrt(X_norm^2+Y_norm^2)
        result <- quantile(dist_norm, p)
    }
    if(kernel=="uniform"){
        b_unif <- sigma/2
        X_unif <- d_ind_MA * round(runif(npoints, min = -b_unif, max = b_unif) / d_ind_MA)
        Y_unif <- d_ind_MA * round(runif(npoints, min = -b_unif, max = b_unif) / d_ind_MA)
        dist_unif <- sqrt(X_unif^2+Y_unif^2)
        result <- quantile(dist_unif, p)
    }
    return(unname(result))
}

#' Finds kernel dispersal parameter
#'
#' Returns dispersal parameter of three types of kernel that
#' accumulates a proportion of the kernel density up to a given distance.
#'
#' @param kernel character, either "normal", "uniform" or "laplacian";
#'     the kernel type. See Rosindell et al. (2008) for details.
#' @param p real 0 < p > 1; the accumulated probability from the kernel
#'     center.
#' @param distance real positive, the quantile that should accumulate
#'     p (for instance, if p=0.5 distance is the median distance)
#' @param density real positive, the density of individual in the
#'     simulation grid, in individuals/hectare
#' @param npoints non-negative integer, number of distance points to
#'     simulate.
#' @param sigma.min real positive, the minimum value of the dispersal
#'     value to try in the one-dimensional optmisation.
#' @param sigma.max real positive, the maximum value of the dispersal
#'     value to try in the one-dimensional optmisation.
#' @return the output of function uniroot, which is a list. The
#'     element 'root' of the list is the dispersal parameter necessary for the kernel having
#'     dist as the quantile for p. For example, p = 0.5  and dist =10
#'     returns the dispersal parameter such that the median dispersal
#'     distance is 10 meters. The solution is not exact because of
#'     rounding values to cell sizes of the simulation grid.
sigkernel <- function(kernel, p, distance, density=20852/50,
                      npoints =1e5, sigma.min = 1, sigma.max= 100){
    f1 <- function(x) distance - qkernel(x, kernel, p, density, npoints)
    uniroot( f1 , lower = sigma.min, upper = sigma.max)
}
  