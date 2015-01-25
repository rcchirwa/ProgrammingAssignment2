## makeCacheMatrix is a utility matrix function can perform operations things:
## 1: Sets a matrix by either A or B 
## sample_matrix <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)

## A.
### cached_sample_matrix <- makeCacheMatrix(sample_matrix)

## OR

## B.
## cached_sample_matrix <- makeCacheMatrix()
## cached_sample_matrix$set(sample_matrix)

## 2: get the values of the matrix:

## cached_sample_matrix$get()


## 3: set  the inverse of the matrix (provided it has been computed already):
## inverse_of_sample_matrix <- solve(sample_matrix) ## compute inverse fo this example
## cached_sample_matrix$setInverse(inverse_of_sample_matrix)

## 4: get  the inverse of the matrix (provided it has been cached already):
##cached_sample_matrix$getInverse()

makeCacheMatrix <- function(x = matrix()) {
		solvedMatrix <- NULL
    set <- function(matrixIn) {
            x <<- matrixIn
            solvedMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(solved) solvedMatrix <<- solved
    getInverse <- function() solvedMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes a matrix that is of type makeCacheMatrix
## will then computes the inverse of the the matrix if it can be inverted 
## then proceeds to cache the object in a technique also know as memoization
## error handling has been added for non-invertable matrices the message will
## be cached to inform the user matrix was not invertible

## the example below is an example of a matrix that can be invereted
## sample_matrix <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
## cached_matrix <- makeCacheMatrix(sample_matrix)
## cacheSolve(cached_matrix)
## cached_matrix$getInverse()


## The example below will not work because the matrix cannot be inverted
## sample_matrix2 <- matrix(c(0, 2, 1, 0,1,2), nrow = 2, ncol = 3, byrow = TRUE)
## cached_matrix2 <- makeCacheMatrix(sample_matrix2) 
## cacheSolve(cached_matrix2)
## cached_matrix2$getInverse()

cacheSolve <- function(x, ...) {

    solvedMatrix <- x$getInverse()
    if(!is.null(solvedMatrix)) {
            message("getting cached data")
            return(solvedMatrix)
    }
    
    data <- x$get()
    
    tryCatch({
      solvedMatrix <- solve(data)
      x$setInverse(solvedMatrix)}, 
      error = function(e) {err_msg <- paste("Could not invert the matrix due to following:\n",e);
                           x$setInverse(err_msg);
                           stop(err_msg); 
            })    
}

