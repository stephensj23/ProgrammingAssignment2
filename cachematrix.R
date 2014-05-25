makeCacheMatrix <- function(x = matrix()) {
        ## X is an invertable matriX 
        ## Returns a matrix that can cache it's inverse
        ##
        ## cache.matrix - matrix that holds the cache
        ## y is a matrix that is passed to the set function.
        ## saveinverse is a matrix that holds the inverted x
        ## getinverse is a matrix that returns the cache.matrix 
 
   
        # clear any prior cached matrices
        cache.matrix <- NULL
        
        #initialize the set function. Calls to makeCacheMatrix$set will create the cached matrix
        set <- function (y){
                x <<- y
                cache.matrix <<- NULL
        }
        
        #initialize the get function. Calls to makeCacheMatrix$get will print out the cached matrix.
        get <- function () x
        
        #creates the inverted matrix
        saveinverse <- function(solve) cache.matrix <<- solve
        getinverse <- function() cache.matrix
        list (set=set,get=get,saveinverse=saveinverse,getinverse=getinverse)
        }    

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         ##
        ## cache.matrix - matrix that holds the cache
        ## saveinverse is a matrix that holds the inverted x
        
        #Sets the cache matrix to the inverse matrix in makeCacheMatrix
        cache.matrix <- x$getinverse()
        
        #Checks to see if the cache.matrix is empty / the inverse has not been calculated.
        if (!is.null(cache.matrix)){
                message ("getting cached data")
                return (cache.matrix)
        }
        
        #Calculates inverse matrix and stores in cache.matrix
        cache.matrix <- solve(x$get(),...)
        
        #Saves the inverse in the matrix
        x$saveinverse (cache.matrix)
        
        #Returns the inverse        
        cache.matrix
}
