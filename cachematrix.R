## TODO
## At present the test to see if the inverse exists in 'cacheSolve'
## is always returning TRUE and then just returning the function defn
## there is therefore a problem with 'makeCacheMatrix'




##  This is a pair of functions that cache the inverse of a matrix.

## For an easy introduction in why you would use the inverse
## of a matrix and how you caluclate it see
## http://www.purplemath.com/modules/mtrxinvr.htm



## This function creates a special "matrix" object
## that can cache its inverse.
## the single argument to the function must be a valid matrix.
## if not, the R attempts to coerce it to a matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize variables
        inverse <- NULL
        
        cache_inverse <- function(inverted) {
          x <<- inverted
          inverse <<- NULL
        }
        # a function that just returns the supplied matrix
        get_matrix <- function() x
        
        solve_inverse <- function(inverse) inverse <<- mean
        
        # function that just returns the inverse of matrix 'x'
        get_inverse <- function() inverse
  
        ## finally, return our list containing the 4 functions
        list(cache_inverse = cache_inverse,
             get_matrix = get_matrix,
             solve_inverse = solve_inverse,
             get_inverse = get_inverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## x will be our special 'matrix' but is really a list
        ## containing 4 functions 
  
        inverse <- x$get_inverse
        
        ## test to see if the inverse is already cached
        ## and, if so, just return it
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        
        ## if inverse not cached we need to obtain the acutal matrix
        mat <- x$get_matrix
        
        ## calculate the inverse
        inverse <- solve(mat)
        
        ## cache this inverse before we return
        x$cache_inverse(inverse)
  
        ## Return a matrix that is the inverse of 'x'
        inverse
}
