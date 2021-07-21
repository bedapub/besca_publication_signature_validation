#' Logical vector of being top or included, and not excluded
#' @param x An atomic vector that can be sorted by \code{sort}, for instance integers and character strings.
#' @param top Integer, number of top elements that we want to consider.
#' @param incFunc Function, applied to \code{x} to return a logical vector of the same length, indicating whether the values should be included even if it does not belong to the top elements.
#' @param excFunc Function, applied to \code{x} to return a logical vector of the same length, indicating whether the values should be excluded even if it does belong to the top elements.
#' @param decreasing Logical, passed to \code{sort}. The default value is set to \code{TRUE}, which means that the highest values are considered the top elements. If set to \code{FALSE}, the lowest values are considered the otp elements.
#' @return A logical vector of the same length as the input \code{x}, indicating whether each element is being either top or included, and not excluded.
#' The function can be used to keep top elements of a vector while considering both inclusion and exclusion criteria.
#' @examples
#' myVal <- c(2, 4, 8, 7, 1)
#' isTopOrIncAndNotExcl(myVal, top=1)
#' isTopOrIncAndNotExcl(myVal, top=3)
#' isTopOrIncAndNotExcl(myVal, top=3, incFunc=function(x) x>=2)
#' isTopOrIncAndNotExcl(myVal, top=3, excFunc=function(x) x%%2==1)
#' isTopOrIncAndNotExcl(myVal, top=3, incFunc=function(x) x>=2, excFunc=function(x) x%%2==1)
#' myVal2 <- c("a", "A", "a", "A", "A")
#' isTopOrIncAndNotExcl(myVal2, 2)
#' isTopOrIncAndNotExcl(myVal2, 2, incFunc=function(x) x=="A")
#' isTopOrIncAndNotExcl(myVal2, 4)
#' isTopOrIncAndNotExcl(myVal2, 4, excFunc=function(x) x=="a")
#' \dontrun{
#' ## the function returns all TRUEs if top is larger than the length of the vector
#' isTopOrIncAndNotExcl(myVal, top=9)
#' }
isTopOrIncAndNotExcl <- function(x, top=1, 
                                 incFunc,
                                 excFunc,
                                 decreasing=TRUE) {
  if(!missing(incFunc)) {
    stopifnot(is.function(incFunc))
    isIncl <- do.call(incFunc, list(x))
  } else {
    isIncl <- rep(FALSE, length(x))
  }
  if(!missing(excFunc)) {
    stopifnot(is.function(excFunc))
    isExcl <- do.call(excFunc, list(x))
  } else {
    isExcl <- rep(FALSE, length(x))
  }
  ind <- order(x, decreasing=decreasing)[1:pmin(top, length(x))]
  isTop <- rep(FALSE, length(x))
  isTop[ind] <- TRUE
  res <- (isTop | isIncl) & !isExcl
  return(res)
}

#' Apply isTopOrIncAndNotExcl filter to a matrix
#' @param matrix A matrix.
#' @param MARGIN Integer, 1 stands for row and 2 stands for column, passed to \code{apply}.
#' @param top Integer, how many top elements should be kept, passed to \code{isTopOrIncAndNotExcl}.
#' @param falseValue The same type as data in the matrix, used to replace values that is \code{FALSE} when judged by \code{isTopOrIncAndNotExcl}.
#' @param ... Further parameters passed to \code{isTopOrIncAndNotExcl}, including \code{incFunc}, \code{excFunc}, and \code{decreasing}.
#' The function applies the filter function \code{isTopOrIncAndNotExcl} to each row or each column to a matrix, keeps the values that are \code{TRUE} based on the logical vector returned by function, and replaces the values that are \code{FALSE} with the value defined by \code{falseValue}.
#' @return A matrix with the same dimnames but with elements not satisfying \code{isTopOrIncAndNotExcl} replaced by \code{falseValue}.
#' @examples
#' myMat <- matrix(c(1,2,3,4,8,7,6,5,12,9,11,10), nrow=3, byrow=TRUE,
#'    dimnames=list(c("A", "B", "C"), c("Alpha", "Beta", "Gamma", "Delta")))
#' print(myMat)
#' applyTopOrIncAndNotExclFilter(myMat, 1, top=2, falseValue=-1) 
#' applyTopOrIncAndNotExclFilter(myMat, 2, top=2, falseValue=-1) 
#' applyTopOrIncAndNotExclFilter(myMat, 2, top=2, falseValue=-1, decreasing=FALSE) 
#' applyTopOrIncAndNotExclFilter(myMat, 1, top=2, falseValue=-1, incFunc=function(x) x%%2==0) 
#' applyTopOrIncAndNotExclFilter(myMat, 1, top=2, falseValue=-1, incFunc=function(x) x%%2==0, excFunc=function(x) x<5) 
applyTopOrIncAndNotExclFilter <- function(matrix, MARGIN,
                                          top=1,
                                          falseValue=0,
                                          ...) {
  if(type(falseValue) != type(matrix)) {
    warning(sprintf("Type of matrix (%s) does not match that of falseValue(%s)\n",
                    type(matrix), type(falseValue)))
  }
  mat <- apply(matrix, MARGIN, function(x) {
    vec <- isTopOrIncAndNotExcl(x, top=top, ...)
    res <- rep(falseValue, length(x))
    res[vec] <- x[vec]
    return(res)
  })
  if(MARGIN==1) {
    mat <- t(mat)
  }
  dimnames(mat) <- dimnames(matrix)
  return(mat)
}