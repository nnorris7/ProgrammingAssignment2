##
## R Programming: rprog-001
## Programming Assignment 2
##
## Norman Norris
## GitHub Account: nnorris7
##

## This file contains 2 functions: makeCacheMatrix and cacheSolve
## The first function creates a special matrix object that can cache its inverse.
## The second function computes the inverse of the special matrix returned by the first
## function. If the inverse has already been calculated (and the matrix has not changed),
## the inverse is retrieved from the cache.

## Function 1: makeCacheMatrix
## This function takes a(n) (invertible) matrix as its argument.
## This function creates a special "matrix" object, which is a list containing functions
## to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function 2: cacheSolve
## This function takes a special matrix object created by Function 1 as its argument.
## This function calculates the inverse of a special "matrix" object created by Function
## 1. However, it first checks to see if the inverse has already be calculated and if so,
## uses get() to set the return value of the inverse from the cache and skip the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the value of
## the inverse in the cache using the setinverse() function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
