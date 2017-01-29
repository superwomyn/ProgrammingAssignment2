## Jana Harper
## Programming Assignment 2: Lexical Scoping
##
## Two functions that create a special matrix object that can cache its
## inverse and fetch the cached matrix (cache hit), or (cache miss) create 
## a new inverted matrix from input and store that matrix in the cache.
##
## Example Usage:
##
## 
## > my_matrix <- matrix(1+1e-10*rnorm(25),nc=5)
## > cacheable_matrix <- makeCacheMatrix(my_matrix)
## > cacheSolve(cacheable_matrix)



# Create a special "matrix" object that can cache itself
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL # initialize variable to store inverse of matrix `x`
    set <- function(y) { 
            x <<- y
            s <<- NULL
    }
    get <- function() x
    set_solve <- function(solve) s <<- solve
    get_solve <- function() s
    list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)
}


# Compute the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has 
# not changed), then the cachesolve should retrieve the inverse from the 
# cache.
cacheSolve <- function(x, ...) {
         s <- x$get_solve() # look for inverse in the cache
        if(!is.null(s)) { # cache hit
                message("getting cached data")
                return(s) # return cached inverse
        }
        # cache miss
        data <- x$get()
        s <- solve(data, ...) # compute inverse
        x$set_solve(s) # cache inverse
        s # return inverse
}
