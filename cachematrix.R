## These are a pair of complementary functions for calculating
## and caching the inverse of an invertible matrix

## The first, 'makeCacheMatrix', provides a wrapper around a matrix
## which provides 3 functions: One which just returns the matrix and
## two 'helper' functions: one which caches the matrix's inverse
## and a second which returns that cached result.

## The second, 'cacheSolve', is then used in place of the base class
## solve() method to calculate or retreive the inverse matrix.
## This function uses the 'helper' methods from 'makeCacheMatrix'
## to either retrieve a cached inverse or, if there isn't one, then
## the inverse is calculated and the result cached.

## For an easy introduction on why you would use the inverse
## of a matrix and how you caluclate it see
## http://www.purplemath.com/modules/mtrxinvr.htm


## makeCacheMatrix
## arguments: an invertible matrix
## Returns a 'list' of functions.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize local 'inverse' to NULL
        ## forces a search to global environment.
        inverse <- NULL
        
        ## return the supplied matrix
        get_matrix <- function() x

        ## cache the inverse
        cache_inverse <- function(inverted) {
           inverse <<- inverted
        }
       
        # return the cached inverse of matrix 'x'
        get_inverse <- function() inverse
  
        ## Return a list containing the 3 functions
        list(cache_inverse = cache_inverse,
             get_matrix = get_matrix,
             get_inverse = get_inverse)
}

## cacheSolve
## arguments: an invertible matrix 'wrapped' inside 'makeCacheMatrix'
## Returns the inverse of the matrix.

## To be used instead of solve() to obtain the inverse

cacheSolve <- function(x, ...) {
  
        ## obtain the inverse
        inverse <- x$get_inverse()
        
        ## test to see if the inverse is already cached
        ## and, if so, just return it
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        
        ## if inverse not cached we need to obtain the acutal matrix
        mat <- x$get_matrix()
        
        ## calculate its inverse
        inverse <- solve(mat)
        
        ## and cache it
        x$cache_inverse(inverse)
  
        ## Return a matrix that is the inverse of the original
        inverse
}

## Sample use
# m <- matrix( c(1,1,1,1,2,1,0,1,2), nrow=3, ncol=3, byrow=TRUE)
# m_cache <- makeCacheMatrix(m)
# cacheSolve(m_cache)

# First time, when there is no cached result, output is:
#   [,1] [,2] [,3]
# [1,]  1.5 -0.5 -0.5
# [2,] -1.0  1.0  0.0
# [3,]  0.5 -0.5  0.5
 
# On subsequent calls to 'cacheSolve(m_cache)' the message
# 'getting cached data'
# is printed prior to the inverse
# indicating that the cached inverse is being returned.
