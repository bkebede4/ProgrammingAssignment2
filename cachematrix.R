## Put comments here that give an overall description of what your
## functions do
## The project consists two functions makeCacheMatrix and cacheSolve.
## The former creates a special matrix that make matrice-inverses and cache, while
## the latter tries to retrieve what's been cached before proceeding to inversing

## Write a short comment describing this function
## The following code has a function: makeCacheMatrix.
## It creates a matrix that can cache its inverse. It includes, set, get, setinv
##, and getinv
library(MASS)    #will enable calculations of non-squared matrices
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL              #creating first the inverse as an empty NULL
        set <- function(y){
                x <<-y
                inv <<- NULL
        }
        get <- function()x      #function to get the x from matrix
        setinv <- function(inverse) inv <<-inverse
        getinv <-function() {
                
                if(!is.null(inv)){    # if used here to avoid unnecessary recalc 
                        message("getting cached data")
                        return(inv) 
                } 
                inv <- ginv(x)   #solves for inverse,including non-square
                inv%*%x           #function used to find the inverse
                inv
        }
        
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}
## Write a short comment describing this function
## the second function, cacheSolve helps get the cache data
## but will proceed to calculate the inverse if not calculated above
cacheSolve <- function(x, ...) {  # gets cached data
        inv <- x$getinv()
        if(!is.null(inv)){   #first checks if inverse is Null
                message("getting cached data")
                return(inv)        }
        data <- x$get()   # else follow by getting x
        inv <- ginv(data, ...)
        x$setinv(inv)
        inv     ## Return a matrix that is the inverse of 'x'
}

The_Matrix <- makeCacheMatrix(matrix(1:6, 3, 2))
The_Matrix$get()
The_Matrix$getinv()
cacheSolve(The_Matrix)

