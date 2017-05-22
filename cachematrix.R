## The following pair of functions cache the inverse of a matrix to ease up the 
## computational burden in a the process.

## The first function is 'makeCacheMatrix' that creates a list containing a function to
# 1. set the value of the matrix
# 2. obtain the value of the matrix
# 3. set the value of inverse of the matrix
# 4. obtain the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following  second function calculates and returns the inverse of the matrix. 
## First, it checks if the inverse matrix has already been computed. 
## If so, it obtains the result and skips the computation. 
## If not, it computes the inverse of the matrix, sets the value in the cache using
## the 'setinverse' function.

## Note that This cacheSolve function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Example

# a<-c(-0.104  0.718  0.507  0.806 -0.073 -0.619  1.725 -0.437 -0.702)
# my_mat<-matrix(a, nrow=3, ncol=3)
# my_mat
# my_matC = makeCacheMatrix(my_mat)
# my_matC$get()
#               [,1]   [,2]   [,3]
#       [1,] -0.104  0.806  1.725
#       [2,]  0.718 -0.073 -0.437
#       [3,]  0.507 -0.619 -0.702

## Result shows no cache in the first run
# cacheSolve(my_matC)
#               [,1]       [,2]       [,3]
#       [1,]  0.4847180  1.1097046  0.5002816
#       [2,] -0.6244804  1.7720482 -2.6376263
#       [3,]  0.9007199 -0.7610792  1.2625832

## Retrieving from the cache in the second run
# cacheSolve(my_matC)

## Now getting the cached data
# getting cached data.
#               [,1]       [,2]       [,3]
#       [1,]  0.4847180  1.1097046  0.5002816
#       [2,] -0.6244804  1.7720482 -2.6376263
#       [3,]  0.9007199 -0.7610792  1.2625832

