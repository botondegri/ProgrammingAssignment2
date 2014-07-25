## These functions handle a special matrix object which helps to cache the
## result of the time consuming calculation of matrix inversion, 2 funtions included:
## 
## - makeCacheMatrix: creates a special object 
## - cacheSolve: returns the inverse matrix of an object created by makeCacheMatrix,
## 	if it's already calculated, then gets it from the object's internal cache, otherwise
##	calculates it and puts it into the objects internal cache


## makeCacheMatrix: creates a special matrix object, the only parameter is the matrix

makeCacheMatrix <- function(x = matrix()) {
##	initial value for internal cache of the inverse matrix
	inv <- NULL

##	function for get the matrix
	get <- function() x

##	function for set the matrix, if the matrix set the cache of the inverse matrix is cleared
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

##	function for set the inverse matrix in internal the cache
	setInv <- function(i) inv <<- i 

##	function for get the inverse matrix from internal the cache
	getInv <- function() inv

## 	cacheMatrix object returned with the functions and the x, inv internal cached objects
	list('setInv' = setInv, 'getInv' = getInv,'set' = set, 'get' = get)
}


## cacheSolve: gets a cacheMatrix object on the input and returns the inverse of the matrix
## if it's already cached then just get it from the cache, otherwise calculates and put it in the cache

cacheSolve <- function(x, ...) {
## 	gets the cache inverse matrix
	inv <- x$getInv()
	if (is.null(inv)) {
## 	if the cache is empty, then inverse has to be calculated and put into the cache
		inv <- solve(x$get())
		x$setInv(inv)
	}

## 	inverse has to be returned
	inv
}
