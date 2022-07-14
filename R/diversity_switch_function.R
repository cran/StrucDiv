#' @name metric 
#' @rdname metric
#' @title Spatial structural diversity metrics
#' @description
#' The functions \code{entropy} , \code{entropyNorm}, \code{contrast}, \code{dissimilarity} and \code{homogeneity}
#' are the spatial structural diversity metrics used in the default configuration of \code{\link{StrucDiv}}. 
#' Structural diversity entropy is entropy with different \code{delta} parameters. Shannon entropy is employed, when \code{delta} = "0". 
#' Shannon entropy has a window-dependent maximum.
#' Additionally, the value gradient is considered when \code{delta} = "1" or \code{delta} = "2". 
#' The values of structural diversity entropy with \code{delta} = "1" or \code{delta} = "2" are not restricted and depend on the values of the input raster.
#' entropyNorm is Shannon entropy normalized over maximum entropy, which depends on the size of the moving window. The metric entropyNorm ranges between 0 and 1.
#' Contrast, dissimilarity and homogeneity are established second-order texture metrics. 
#' Contrast and dissimilarity consider the value gradient, their values are not restricted and depend on the values of the input raster.
#' Homogeneity quantifies the closeness of empirical probabilities to the diagonal and ranges between 0 and 1. 
#' Homogeneity is 1 when all pixel pairs are the same and approaches  0 as differences increase.
#' @param rank logical. Should values be replaced with ranks in each gray level co-occurrence 
#' matrix (GLCM)? Defaults to FALSE.
#' @param delta character, takes 3 options: "0", "1", or "2". 
#' Delta is the difference weight parameter, 
#' it defines how the differences between pixel values within a pixel pair should be weighted.  
#' If rank = TRUE, delta defines how the differences between ranks should be weighted.  
#' The default value is "0" (no weight). Set delta = "1" for absolute difference weight, 
#' or delta = "2" for squared difference weight. 
#' The delta parameter can only be set when the metric entropy is used. 
#' Dissimilarity automatically employs delta = "1", and contrast employs delta = "2".
#' @param SpatMat the GLCM that is returned by an internal function
#' to the \code{StrucDiv} function. 
#' @param Hetx the spatial structural diversity matrix that is returned by an internal function
#' to the \code{StrucDiv} function. the spatial structural diversity metric is calculated on every element
#' of the GLCM, which generates the spatial structural diversity matrix \code{Hetx}. The sum of this
#' matrix represents the spatial structural diversity estimate of the moving window, 
#' the size of which is defined by \code{wsl} in the \code{StrucDiv} function.
#' @param nrp integer. The total number of pixel pairs, which is calculated 
#' internally and passed to the spatial structural diversity metric functions.
#' The total number of pixel pairs is defined by the size of the moving window (\code{wsl x wsl}), 
#' and by the angle.
#' @param narm logical. Defines how missing values are treated, and is automatically set to 0 if na.handling = na.pass, and to 1 if na.handling = na.omit.
#' @param display_progress logical. If TRUE the progress bar is displayed.
#' @param ... possible further arguments.
#' @details This function is used internally and is called as an argument to \code{StrucDiv}.
#' @return The output is a raster layer with the same dimensions as the input raster and is called a (spatial) structural diversity map. 
#' It represents spatial structural diversity quantified on the spatial scale that is defined by the size of the moving window. 
#' When \code{na.handling = na.pass}, then the output map will have an NA-edge of 0.5*(\code{wsl}-1), 
#' and it will contain more missing values than the input raster.
#' @importFrom raster raster
#' @export

entropy <- function(rank, delta, Hetx, SpatMat, nrp, narm, display_progress, ...) {
  
  rank_delta <- paste(rank, delta)
  
  switch_function <- function(rank_delta) {
    
    switch(EXPR = rank_delta,
           "FALSE 0" = .Entropy(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress),
           "TRUE 0" = .Entropy(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress),
           "TRUE 1" = .WeightedEntropyAbsRank(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress),
           "FALSE 1" = .WeightedEntropyAbsValue(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress),
           "TRUE 2" = .WeightedEntropySqrRank(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress),
           "FALSE 2" = .WeightedEntropySqrValue(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress)
    )
  }
  
  v <- switch_function(rank_delta)
  return(v)
  
}


#' @rdname metric
#' @export

entropyNorm <- function(rank, delta, Hetx, SpatMat, nrp, narm, display_progress, ...) {
  
  v <- .NormalizedEntropy(Hetx = Hetx, PMat = SpatMat, nrp = nrp, narm = narm, display_progress = display_progress)
  
  return(v)
  
}

#' @rdname metric 
#' @export

contrast <- function(rank, delta, Hetx, SpatMat, nrp, narm, display_progress, ...) {
  
  switch_function <- function(rank) {
    
    switch(EXPR = as.character(rank),
           "TRUE" = .ContrastRank(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress),
           "FALSE" = .ContrastValue(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress)
    )
  }
  
  v <- switch_function(rank)
  return(v)
  
}

#' @rdname metric
#' @export

dissimilarity <- function(rank, delta, Hetx, SpatMat, nrp, narm, display_progress, ...) {
  
  switch_function <- function(rank) {
    
    switch(EXPR = as.character(rank),
           "TRUE" = .DissimilarityRank(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress),
           "FALSE" = .DissimilarityValue(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress)
    )
  }
  
  v <- switch_function(rank)
  return(v)
  
}

#' @rdname metric
#' @export

homogeneity <- function(rank, delta, Hetx, SpatMat, nrp, narm, display_progress, ...) {
  
  switch_function <- function(rank) {
    
    switch(EXPR = as.character(rank),
           "TRUE" = .HomogeneityRank(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress),
           "FALSE" = .HomogeneityValue(Hetx = Hetx, PMat = SpatMat, narm = narm, display_progress = display_progress)
    )
  }
  
  v <- switch_function(rank)
  return(v)
  
}

#' @rdname metric
#' @export




