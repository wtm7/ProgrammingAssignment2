## makeCacheMatrix makes a special type of matrix object that stores a cached
## copy of the inverted matrix

## usage:
##       with an existing matrix, x, e.g. x <- matrix(c(3,2,3,4,5,6,7,8,9),3,3)
##		qq <- makeCacheMatrix(x)
##	or, with a matrix being described at the of this funciton call
##		qq <- makeCacheMatrix(matrix(c(3,2,3,4,5,6,7,8,9),3,3))
## once the special object is created,
## qq$get() -- returns the value of the matrix stored in this object
## qq$set(x) -- initializes or updates the value of the matrix stored
##		and, two other functions are defined for internal use only,
##			qq$setinv(x) -- to store the inverted matrix
##			qq$getinv() -- to fetch the previously stored inverted matrix
## qqinv <- cacheSolve(qq) -- calculates the inverted matrix, either from
##			output of solve(), or the previously cached calculation
##
##
##
## makeCacheMatrix -- makes the special object holding the matrix to be invertd
##
##	parameter: x -- matrix to be stored in this object, or empty
##
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) { x <<- y ; m <<- NULL }
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set=set,get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve -- returns the inverse of the matrix, either by calculation or
##		cached from a previous calculation
##
##	parameter: x -- name of the special object holding the matrix to be inverted
##
cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if (!is.null(m)) { message("getting cached data"); return(m) }
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}

