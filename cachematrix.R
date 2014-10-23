## Following are two functions that will be used to cache the inverse of a matrix


## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{

	i <- NULL
	## Set Method 
	set <- function( matrix ) 
	{
		m <<- matrix
		i <<- NULL
	}
	## Get Method 
	get <- function() 
	{
		m
	}
	## Set Method to Inverse
	setInverse <- function(inverse) 
	{
		i <<- inverse
	}
	
	## Get Method to Inverse
	getInverse <- function()
	{
		i
	}
	## A list of the methods
	list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
	m <- x$getInverse()
	## Return the inverse if its already set
	if( !is.null(m) ) 
	{
		message("getting cached data")
		return(m)
	}
	## Get the matrix from our object
	data <- x$get()
	
	## Calculate the inverse using matrix multiplication
	m <- solve(data) %*% data
	x$setInverse(m)
	
	m
}
