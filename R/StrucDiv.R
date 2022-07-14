#' @name StrucDiv
#' @rdname StrucDiv
#' @title Calculate spatial structural diversity for an arbitrary raster layer. 
#' @description
#' This is a wrapper function that returns a spatial structural diversity map 
#' as a raster layer. 
#' Pixels are considered as pairs in user-specified distances and angles. 
#' Angles include horizontal and vertical direction, and the diagonals at 45° and 135°. 
#' The direction-invariant version considers all angles. 
#' The frequencies of pixel pairs are normalized by the total number of pixel pairs, 
#' which returns the gray level co-occurrence matrix (GLCM). 
#' The total number of pixel pairs depends on the extent of the area within which pixel pairs are counted, i.e. on the spatial scale. 
#' The spatial scale is defined by the window side length (\code{wsl}) of a moving window. 
#' The values in a GLCM are the same values that occur in the area within which pixel pairs were counted, 
#' therefore they can differ between GLCMs.
#' In each GLCM, pixel values can be replaced with ranks.
#' Diversity metrics are calculated on every element of the GLCM, 
#' their sum is assigned to the center pixel of the moving window 
#' and represents spatial structural diversity of the area captured by the moving window. 
#' The final map is called a '(spatial) structural diversity map' and is returned as a raster layer 
#' with the same dimensions as the input raster.
#' @param x raster layer. Input raster layer for which 
#' spatial structural diversity should be calculated.
#' @param wsl uneven integer. The window side length, 
#' \code{wsl} x \code{wsl} defines the size of the moving window.
#' The window must be smaller than the dimensions of the input raster. 
#' The moving window defines the spatial scale on which spatial structural diversity is quantified.
#' @param dist integer. The distance between two pixels that should be considered as a pair, 
#' defaults to dist = 1 (direct neighbors).
#' @param angle string. The angle on which pixels should be considered as pairs. 
#' Takes 5 options: "horizontal", "vertical", "diagonal45", "diagonal135", "all". 
#' "all" is the direction-invariant version that considers all 4 angles. Defaults to "all".
#' @param rank logical. Should pixel values be replaced with ranks in each GLCM? Defaults to FALSE.
#' @param fun function, the diversity metric. Takes one of the following: entropy,
#' entropyNorm, contrast, dissimilarity, or homogeneity. 
#' Structural diversity entropy is entropy with different \code{delta} parameters. Shannon entropy is employed, when \code{delta} = "0". 
#' Shannon entropy has a scale-dependent maximum.
#' Additionally, the value gradient is considered when \code{delta} = "1" or \code{delta} = "2". 
#' The values of structural diversity entropy with \code{delta} = "1" or \code{delta} = "2" are not restricted and depend on the values of the input raster.
#' entropyNorm is Shannon entropy normalized over maximum entropy, which depends on the size of the moving window. The metric entropyNorm ranges between 0 and 1.
#' Contrast, dissimilarity and homogeneity are established second-order texture metrics. 
#' Contrast and dissimilarity consider the value gradient, their values are not restricted and depend on the values of the input raster.
#' Homogeneity quantifies the closeness of empirical probabilities to the diagonal and ranges between 0 and 1. 
#' Homogeneity is 1 when all pixel pairs are the same and approaches  0 as differences increase.
#' @param delta character, takes three options: "0", "1", or "2". 
#' Delta is the difference weight, 
#' it defines how the differences between pixel values within a pixel pair should be weighted.  
#' If rank = TRUE, delta defines how the differences between ranks should be weighted.  
#' The default value is "0" (no weight). Set delta = "1" for absolute difference weight, 
#' or delta = "2" for squared difference weight. 
#' The delta parameter can only be set when the metric entropy is used. 
#' Dissimilarity automatically employs delta = "1", and contrast employs delta = "2".
#' @param na.handling na.omit or na.pass. 
#' If na.handling = na.omit, NAs are ignored, diversity metrics are calculated with less values. 
#' In this case the GLCM does not sum to 1.
#' If na.handling = na.pass and if there is at least one missing value inside the moving window,
#' an NA is assigned to the center pixel. Therefore, the diversity map will contain more 
#' NAs than the input raster layer.
#' Defaults to na.pass.
#' @param padValue numeric. The value of the padded cells at the edges of the input raster. 
#' Defaults to NA.
#' @param aroundTheGlobe logical. If the input raster goes around the whole globe, 
#' set aroundTheGlobe = TRUE, and the input raster will be "glued together" from both sides
#' to calculate spatial structural diversity without edge effects on the sides.
#' Defaults to FALSE.
#' @param filename character. If the output raster should be written to a file, define file name (optional).
#' @param verbose logical. If TRUE a progress bar is printed
#' @param ... possible further arguments.
#' @importFrom raster raster
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @importFrom stats na.omit na.pass
#' @useDynLib StrucDiv, .registration = TRUE
#' @details The memory requirement of the function is determined 
#' by \code{raster::canProcessInMemory()}. 
#' If the raster file cannot be processed in memory, its size needs to be reduced before \code{StrucDiv} can be used. 
#' @return The output is a (spatial) structural diversity map, returned as a raster layer with the same dimensions as the input raster.
#' When \code{na.handling = na.pass}, then the output map will have an NA-edge of 0.5*(\code{wsl}-1), 
#' and it will contain more missing values than the input raster.
#' The output represents spatial structural diversity quantified on a spatial scale defined by the 
#' size of the moving window.
#' @examples 
#' # Calculate contrast on a small raster file with random normal distribution
#' a <- raster::raster(matrix(rnorm(648), 18, 36))
#' raster::plot(a)
#' contrast_a <- StrucDiv(a, 3, fun = contrast, na.handling = na.omit, rank = FALSE)
#' raster::plot(contrast_a)
#' 
#' # Calculate dissimilarity on a small raster file with random normal distribution
#' b <- raster::raster(matrix(rnorm(100), 10, 10))
#' raster::plot(b)
#' dissim_b <- StrucDiv(b, 5, fun = dissimilarity, na.handling = na.pass, rank = FALSE)
#' raster::plot(dissim_b)
#'
#' @export

StrucDiv <- function(x, wsl, dist = 1, angle = c("all", "horizontal", "vertical", "diagonal45", "diagonal135"),
                     rank = FALSE, fun, delta = c('0', '1', '2'),
                     na.handling = na.pass, padValue = NA,
                     aroundTheGlobe = FALSE, filename = "", 
                     verbose = TRUE, ...) {  
  
  dotArgs <- list(...)
  
  angle <- match.arg(angle)  
  delta <- match.arg(delta)
  
  if(!is.function(na.handling)){
    stop("na.handling must be either 'na.omit' or 'na.pass'")
  }
  
  if(!(identical(na.handling, na.pass) | identical(na.handling, na.omit))){
    stop("na.handling must be either 'na.omit' or 'na.pass'")
  }
  
  if(isTRUE(aroundTheGlobe) & !isTRUE(.isGlobalLonLat(x))){
    warning("The raster image does not go around the globe.")
  }
  
  suppressWarnings(
    if (identical(na.handling, na.pass) && anyNA(raster::values(x)) ) {
      warning("Raster layer contains missing values. Wherever there are missing values,
             an NA will be returned. if you want to proceed without NAs, 
             set 'na.handling = na.omit'.")
    }
  )
  
  out <- raster::raster(x)
  
  stopifnot(raster::hasValues(x))
  
  if (wsl <= 0) {
    stop("Window side length must be > 0.")
  }
  if (wsl %% 2 == 0) {
    stop("Window side length must be an odd number.")
  }
  if (wsl > min(dim(out)[1:2])) {
    stop("Window must be smaller than the raster dimensions.")
  }
  
  if (dist > min(dim(out)[1:2]) / 2 - 1) {
    stop("Distance value is too big.")
  }
  
  if (angle != match.arg(angle)) {
    stop('Angle must be one of "horizontal", "vertical", "diagonal45", "diagonal135", or "all".')
  }  
  
  if (delta != match.arg(delta)) {
    stop('Angle must be one of "0", "1", or "2".')
  }
  
   if(!is.function(fun)){
    stop("This diversity metric is not available.")
   }
  
  if(!(identical(fun, entropy) | identical(fun, entropyNorm) | identical(fun, contrast) | identical(fun, dissimilarity) | identical(fun, homogeneity))){
    stop("fun must be one of entropy, entropyNorm, contrast, dissimilarity or homogeneity.")
  }
  
  if (raster::canProcessInMemory(out)) {
    
    if(verbose == TRUE) {
    vMat <- .getValuesWindow(x, wsl = wsl, padValue = padValue, 
                                  aroundTheGlobe = aroundTheGlobe)
    }
    else{
      suppressMessages(vMat <- .getValuesWindow(x, wsl = wsl, padValue = padValue, 
                                                aroundTheGlobe = aroundTheGlobe))
    }
    
    Hetx <- vMat
    
    suppressWarnings(
      if( identical(na.handling, na.omit) && anyNA(raster::values(x)) ) {
        
        narm <- 1
        
      } 
      else{
        
        narm <- 0
      
      }
    )
    
    
    switch_angle <- function(angle) {
      
      switch(angle,
             "horizontal" = .ProbabilityMatrixHorizontalDynamic(vMat = vMat, d = dist, narm = narm,
                                                                display_progress = verbose),
             "vertical" = .ProbabilityMatrixVerticalDynamic(vMat = vMat, d = dist, narm = narm,
                                                            display_progress = verbose),
             "diagonal45" = .ProbabilityMatrixDiagonal45Dynamic(vMat = vMat, d = dist, narm = narm,
                                                                display_progress = verbose),
             "diagonal135" = .ProbabilityMatrixDiagonal135Dynamic(vMat = vMat, d = dist, narm = narm,
                                                                  display_progress = verbose),
             "all" = .ProbabilityMatrixAllDynamic(vMat = vMat, d = dist, narm = narm,
                                                  display_progress = verbose),
             .ProbabilityMatrixAllDynamic(vMat = vMat, d = dist, narm = narm,
                                         display_progress = verbose)
      )
      
    }
    
    SpatMat <- switch_angle(angle)
    
    if (angle %in% c("horizontal", "vertical")) {
      nrp <- 2*wsl*(wsl - dist)
    }
    
    if (angle %in% c("diagonal45", "diagonal135")) {
      nrp <- (wsl - dist) * 2 *(wsl - dist)
    }
    
    if (angle == "all") {
      nrp <- 4 * (wsl - dist) * (2 * wsl - dist)
    }
    
    v <- do.call(fun, list(rank = rank, Hetx = Hetx, SpatMat = SpatMat, delta = delta,
                           nrp = nrp, narm = narm, display_progress = verbose))
    
    out <- raster::setValues(out, v)
    return(out)
    
    if (filename != "") {
      out <- raster::writeRaster(out, filename)
    }
    
  } else {
    stop("Cannot proccess in memory.")
  }
  
}
