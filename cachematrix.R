## This pair of functions (makeCacheMatrix and cacheSolve) cache the inverse of a matrix.
## makeCacheMatrix: creates a "special" matrix object and object functions ("methods"), which will be accessed and 
##                  used by cacheSolve 
## cacheSolve: computes, stores (i.e, cache) and retrieves the inverse.



## The fuction makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This function receives (i.e., input) a matrix and returns a list of objects, which in this
## case it's a list of function objects ("methods").
## The matrix object and functions objects ("methods") will be accessed and used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {        # input 'x' of the function is a matrix  
        
        inv.m <- NULL                              # 'inv.m' is the "inverse" of the matrix.
                                                   # 'inv.m' is reset to NULL every time          
                                                   #  makeCacheMatrix is called 
                                                   # (i.e., when a new matrix object is created)                                                   
        
        
        set <- function(y) {                       # 'y' is a free variable (within the makeCashMatrix)
                                                   # and wil be found in the user's workspace
                
                x <<- y                            # the function 'set'takes the input'y' and stores it in
                                                   # 'x'(which is a new input to the matrix object),
                                                   # within the evironment of makeCacheMatrix.
                
                                                   # If 'set' is called externally (i.e.from the user's
                                                   #  workspace),then, the value entered in 'set' will be
                                                   # used as a new input to the matrix object
                                                   # (i.e., matrix value will change)    
                
                
                inv.m <<- NULL                     # reset the inverse to NULL (i.e., as we changed the 
                                                   # value of the matrix - 'y' input to 'x')
        }                                                        
        
        
        get <- function() x                        # 'get' returns the current value of the matrix object  
        
        setsolve <- function(solve)                # 'setsolve' is called by cacheSolve() access  
                
                {inv.m <<- solve}                    #  stores the value of the inverse of the matrix
                                                           
        
        getsolve <- function() inv.m               # 'getsolve' returns the cached value of the inverse of
                                                   #  the matrix to cacheSolve() on subsequent accesses
                                                   #  (if matrix value has not been changed).  
                                                   #  If the value of the matrix object changed,
                                                   #  'getsolve' will retun NULL  
        
        
        list(set = set, get = get,                 # This list of funcion object is returns every time the
                                                   #  makeCacheMatrix is called (i.e., creating a new "special" 
                                                   #  matrix object.)
             setsolve = setsolve,                  # It lists the functions ("methods") that are part 
                                                   # of the object.
             getsolve = getsolve)                  # If a function is not on the list it cannot be 
                                                   # accessed by cacheSolve (or externally) 
        
}




## The function cacheSolve computes the inverse of the "special" matrix object that is returned by makeCacheMatrix,
## stores (i.e., cache) it and then retruns it.
## If the inverse has already been calculated (i.e., matrix value has not been changed), then the
## cacheSolve fetches the inverese that was stored in the cache earlier and returns it.
## NOTE: Within this pair of functions,there are two ways to change the value of the matrix:
##     (1) calling makeCacheMatrix will create a new object (i.e., a new "special" matrix object.)
##     (2) calling the "method" set() externally. This will chnage the value of the existing object
##         (but will NOT create a new object.)   


cacheSolve <- function(x, ...) {                    # the input is the object cretaed by makeCacheMatrix
        
        inv.m <- x$getsolve()                       # access the object 'x' and gets the value of its 
                                                    # inverse (i.e., inverse of the matrix) 
        
        if(!is.null(inv.m)) {                       # if the inverse was already cached (i,e, stored);
                                                    # which means that 'inv.m' is not NULL
                
                message("getting cached data")      # sends this message to the console
                return(inv.m)                       # and returns the inverse value stored in the cache
        }
        
        
                                                    # we reach the following code ONLY if x$getsolve returns
                                                    # NULL (i.e., the value of the matrix changed)
        
        
        data <- x$get()                             # gets the value of the new matrix (i.e., either when 
                                                    # makeCacheMatrix is called or set() "method" is called externally)  
        
        inv.m <- solve(data, ...)                   # computes the inverse for the new matrix 
        
        x$setsolve(inv.m)                           # stores the calculated inverse value in 'inv.m' 
        
        inv.m                                       # returns the inverse  
}