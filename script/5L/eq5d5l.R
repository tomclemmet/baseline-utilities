eq5d5l <- function(mo, sc, ua, pd, ad) {
  value_set <- list(
    mobility = c(0, 0.032, 0.058, 0.0179, 0.279),
    selfcare = c(0, 0.038, 0.060, 0.162, 0.206),
    usualact = c(0, 0.049, 0.086, 0.184, 0.212),
    paindis = c(0, 0.056, 0.066, 0.371, 0.479),
    anxdep = c(0, 0.041, 0.126, 0.313, 0.391)
  )
  index <- 1 -
    value_set$mobility[mo] -
    value_set$selfcare[sc] -
    value_set$usualact[ua] -
    value_set$paindis[pd] -
    value_set$anxdep[ad]
  return(index)
}
