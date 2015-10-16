makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample run:
x = rbind(c(2, 1), c(1, 2))
m = makeCacheMatrix(x)
m$get()

##No cache in the first run
cacheSolve(m)

## Retrieving from the cache in the second run
cacheSolve(m)

> makeCacheMatrix <- function(x = matrix()) {
+ inv <- NULL
+ set <- function(z) {
+ x <<- z
+ inv <<- NULL
+ }
+ get <- function() x
+ setinverse <- function(inverse) inv <<- inverse
+ getinverse <- function() inv
+ list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
+ }
> cacheSolve <- function(x, ...) {
+ inv <- x$getinverse()
+ if(!is.null(inv)) {
+ return(inv)
+ }
+ data <- x$get()
+ inv <- solve(data)
+ x$setinverse(inv)
+ inv
+ }
> ## Sample run:
> x = rbind(c(2, 1), c(1, 2))
> m = makeCacheMatrix(x)
> m$get()
     [,1] [,2]
[1,]    2    1
[2,]    1    2
> ##No cache in the first run
> cacheSolve(m)
           [,1]       [,2]
[1,]  0.6666667 -0.3333333
[2,] -0.3333333  0.6666667
> ## Retrieving from the cache in the second run
> cacheSolve(m)
           [,1]       [,2]
[1,]  0.6666667 -0.3333333
[2,] -0.3333333  0.6666667
> 