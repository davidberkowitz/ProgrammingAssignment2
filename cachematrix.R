## This file implements a new "class" of matrix that stores its own inverse
## This is intended to speed up requests for the inverse since it
## eliminates the need to calculate it each and every time
## The stored inverse is reset if the matrix itself is changes

## Create a new CacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return the inverse of the CacheMatrix
## If the inverse is NULL (using get), calculate it using solve()
## And save it for later (using setinverse)

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i        
}

## This is how I tested

testAssignment <- function() {

	# establish a random square array of integers
	# (assume it is invertible, but will know for sure when dd is calculated)
	
	dim = sample(25,1)
	cat ("array dimension is:\n", dim, "\n")
	cat ("\n")
		
	d = matrix(sample(1000,dim*dim), nrow=dim, ncol=dim)
	cat ("array is:\n")
	print (d)
	cat ("\n")
	
	# calculate the inverse directly
	dd = solve(d)

	# now use the functions under test
	a = makeCacheMatrix(d)
	aa = cacheSolve(a)
	
	# aa should equal dd
	cat ("dd - aa should equal a ZERO matrix\n")
	print (dd - aa)
	cat ("\n")
	
	# d %*% a should be identity matrix
	cat ("d * aa should equal an IDENTITY matrix\n")
	print (round(d %*% aa))
	cat ("\n")
	
}