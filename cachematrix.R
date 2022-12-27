## makeCacheMatrix creates a matrix and its inverse


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##initial call for the matrix funciton from the vector example in assignment instructions
	        inv <- NULL ##this stores the matrix for the next function (cacheSolve)
        set <- function(y) {	##for the parent function and value of matrix
                x <<- y
                inv <<- NULL ##if the matrix is new inv is NULL
        }
        get <- function() x ##function to get the inverse
        setinverse <- function(inverse) inv <<- inverse ##assigns inv as the inverse of the matrix
        getinverse <- function() inv ##gets the value of the inverse
        list(set = set, get = get, ##list is used for the $ operator in the next function to check the cache
             setinverse = setinverse,
             getinverse = getinverse)
}


##the following formula checks if the matrix has its inverse
##it retrieves the cache of the previous function to save on computation if previous matrix is not modified


cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) ## returns cached data of previous unmodified matrix
        }
        data <- x$get()
        inv <- solve(data, ...) ##calculates the inverse value
        x$setinverse(inv) ##sets inverse in the previous function
        inv
}
