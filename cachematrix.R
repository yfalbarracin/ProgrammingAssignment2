## makeCacheMatrix creates a special matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get <- function() x
	setsolve<-function(solve)m<<-solve
	getsolve<-function() m
	list(set=set,get_get,setsolve=setsolve,getsolve=getsolve)
}


## cacheSolve computes the inverse of the matrix from the above function, if it has already been solved then it retrieves it from the cache.  ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       m<-x$getsolve()
       if(!is.null(m)) {
       	message("getting cached data")
       	return(m)
       }
       data<-x$get()
       m<-solve(data, ...)
       x$setsolve(m)
       m
       
}
