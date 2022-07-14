## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figure/StrucDiv-",
  out.width = "100%"
)

## ----data, echo=FALSE, message = FALSE, eval = FALSE--------------------------
#  library(raster)
#  library(StrucDiv)
#  ndvi.15gl <- raster(ndvi.15gl)
#  extent(ndvi.15gl) <-  c(165.0205, 174.8301, 66.62804, 68.61332)

## ----example, echo=TRUE, message = FALSE, eval=FALSE--------------------------
#  
#  entNorm <- StrucDiv(ndvi.15gl, wsl = 5, dist = 1, angle = "all", fun = entropyNorm,
#                    na.handling = na.pass)
#  

