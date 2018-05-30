##create a function that caches the inverse of a matrix 

##create a special "matrix" caching its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solveMatrix) inv <<- solveMatrix
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##compute the inverse of the matrix created above
cacheSolve <- function(Kobe, ...) {
        ## Return a matrix that is the inverse of 'Kobe'
        inv <- Kobe$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- Kobe$get()
        inv <- solve(data)
        Kobe$setInverse(inv)
        inv 
}
