#' make_euclidean_distance:  define a function to make the euclidean_distance
#'
#' @param max_bias the maximum of the bias parameter used to make the euclidean distance
#' @param max_se the maximum of the SE parameter used to make the euclidean distance

make_euclidean_distance = function(max_bias, max_se){
  euclidean_distance = expand.grid(x = seq(0, max_bias, length.out=100),
                                   y = seq(0, max_se, length.out=100))
  euclidean_distance$z = with(euclidean_distance, sqrt(x^2 + y^2))
  return(euclidean_distance)
  
}