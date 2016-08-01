## These R functions were created in order to complete my Programming Assingment 2 of Week 3 of
## R Programming course in Coursera.
## Both functions deal with with the matrix inverse computation. In the first one we create a matrix, from
## which we can cache its inverse and in the second one we compute the inverse of this cached matrix.

## The following function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inverse_x <<-inverse
        getinverse <- function() inverse_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getinverse()
        if (!is.null(inverse_x)) {
                message("getting cached inverse matrix")
                return(inverse_x)
        } else {
                inverse_x <- solve(x$get())
                x$setinverse(inverse_x)
                return(inverse_x)
        }
}
