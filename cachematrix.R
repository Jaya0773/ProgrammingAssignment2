## Put comments here that give an overall description of what your
## functions do
## There are two functions 
## 1.makeCacheMatrix - This will create a list of functions to set the original matrix,
##                     set the inverse and get the inverse from the cache
## 2.cacheSolve      - This will get the inverse of the matrix from cache (if available), else 
##                     will calculate the inverse and store in cache

## Write a short comment describing this function
## The makeCacheMatrix returns the list of following functions
## 1. setmatx    - Sets the data for retrieval
## 2. getmatx    - Gets the data for inverse calculation
## 3. setinverse - Sets the inverse of the matrix 
## 4. getinverse - Gets the inverse of the matrix
## 5. getorigmx  - Gets the original matrix for which inverse was calculated
makeCacheMatrix <- function(x = matrix()) {
        mxinv <- NULL
        mx <- NULL
        setmatx <- function(y) {
                x <<- y
                mxinv  <<- NULL
                mx <<- NULL
        }
        getmatx <- function() x
        setinverse <- function(inverse, matx){ 
         mxinv  <<- inverse
         mx     <<- matx }
        getinverse <- function() mxinv
        getorigmx  <- function() mx
        list( getmatx = getmatx, setmatx = setmatx,
              getinverse = getinverse, setinverse = setinverse,getorigmx = getorigmx)
}


## Write a short comment describing this function
## This function will check whether the inverse has been cached 
## and if the matrix has not changedand if so will retrieve it from memory, 
## otherwise will set the inverse for future retrieval

cacheSolve <- function(x, ...) {
        mxinv  <- x$getinverse()
        mx     <- x$getorigmx()
        if ((!is.null(mx)) && identical(mx, x$getmatx())) {
          
                message("getting cached data")
                return(mxinv)
            
        }
        data <- x$getmatx()
        mxinv <- solve(data)
        x$setinverse(mxinv, data)
        mxinv
}
