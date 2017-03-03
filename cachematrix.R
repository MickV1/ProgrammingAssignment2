> # Matrix inversion is usually a costly computation, hence it is
> # beneficial to cache the inverted matrix rather than to compute it 
> # repeatedly. 
> # The following two functions "makeCacheMatrix" and "cacheSolve"
> # are used to cache the inverse of a given matrix.
> 
> # Function "makeCacheMatrix" consists of nested functions that:
> # a) Set the value of the matrix
> # b) Get the value of the matrix
> # c) Set the value of inverse of the matrix
> # d) Get the value of inverse of the matrix
> 
> makeCacheMatrix <- function(x = matrix()) {
+         inv <- NULL
+         set <- function(y) {
+                 x <<- y
+                 inv <<- NULL
+         }
+         get <- function() x
+         setInverse <- function(inverse) inv <<- inverse
+         getInverse <- function() inv
+         list(set = set,
+              get = get,
+              setInverse = setInverse,
+              getInverse = getInverse)
+ }
> 
> # Bellow function verifies whether the inverse matrix has already being 
> # computed. 
> # If so, it will use that inverse matrix instead of computing it
> # unnecesarrily.
> # If not, it will compute the inverse and set it in the cache via setInverse
> # function.
> 
> cacheSolve <- function(x, ...) {
+         ## Return a matrix that is the inverse of 'x'
+         inv <- x$getInverse()
+         if (!is.null(inv)) {
+                 message("getting cached data")
+                 return(inv)
+         }
+         mat <- x$get()
+         inv <- solve(mat, ...)
+         x$setInverse(inv)
+         inv
+ }
> 
> my_matrix <- makeCacheMatrix(matrix(c(4,1,3,1),2,2))
> my_matrix$get()
     [,1] [,2]
[1,]    4    3
[2,]    1    1
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]    1   -3
[2,]   -1    4
> my_matrix$getInverse()
     [,1] [,2]
[1,]    1   -3
[2,]   -1    4
> 
> ## Verification that an inverse matrix was computed
> ## (multiplication of a matrix and its inverse matrix 
> ## should result in an Identity matrix)
> 
> a <-my_matrix$get()
> b <-cacheSolve(my_matrix)
getting cached data
> a %*% b
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> 
> # The above matrix is indeed an Identity matrix
> 
> my_matrix$set(matrix(c(4,3,3,2),2,2))
> my_matrix$get()
     [,1] [,2]
[1,]    4    3
[2,]    3    2
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2    3
[2,]    3   -4
> my_matrix$getInverse()
     [,1] [,2]
[1,]   -2    3
[2,]    3   -4
> 
> p <- my_matrix$get()
> q <- cacheSolve(my_matrix)
getting cached data
> p %*% q
     [,1] [,2]
[1,]    1    0
[2,]    0    1
