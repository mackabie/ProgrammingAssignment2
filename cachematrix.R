## These pair of functions (makeCacheMatrix and cacheSolve) cache the inverse of a matrix.
## makeCacheMatrix: creates an object "matrix" and object functions ("methods"), which will be used by cachSolve 
## cacheSolve: computes, stores (i.e, cache) and retrieves the inverse.



## The fuction makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This function recieves (i.e., input) a matrix and returns a list of objects, which in this
## case it's a list of function objects ("methods").
## The object "matrix and object functions will be accessed and used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {        # input 'x' of the function is a matrix  
        
        inv.m <- NULL                              # 'inv.m' is the "inverse" of the matrix.
                                                   # 'inv.m' is reset to NULL every time          
                                                   #  makeCacheMatrix is called 
                                                   # (i.e., when a new matrix is created                                                   
        
        
        set <- function(y) {                       # 'y' is a free variable (within the makeCashMatrix)
                                                   # and wil be found in the user's workspace
                
                x <<- y                            # the function 'set'takes the input'y' and stores it in
                                                   # 'x'(which is a new input to makeCacheMatrix),
                                                   # within the evironment of makeCacheMatrix
                
                                                   # if 'set' is called externally (i.e.from the user's
                                                   #  workspace),then, the value entered in 'set' will be
                                                   # used as a new input to the makeCacheMatrix function
                                                   # (i.e., matrix value will change)    
                
                
                inv.m <<- NULL                     # reset the inverse to NULL (i.e., as we changed the 
                                                   # value of the matrix - 'y' input to 'x')
        }                                                        
        
        
        get <- function() x                        # 'get' returns the value of the original matrix  
        
        setsolve <- function(solve)                # 'setsolve' is called by cacheSolve() access  
                
                inv.m <<- solve                    #  stores the value of the inverse of the matrix
                                                   # (using superassignment)
        
        
        getsolve <- function() inv.m               # 'getsolve' returns the cached value of the inverse of
                                                   #  the matrix to cacheSolve() on subsequent accesses
                                                   #  (if no new matrix was entered).  
                                                   #  If a new value (matrix) is input to makeCasheMatrix,
                                                   #  'getsolve' will retun NULL  
        
        
        list(set = set, get = get,                 # This list of funcion object is returns every time the
                                                   #  makeCacheMatrix is called (i.e., a new matrix.)
             setsolve = setsolve,                  # It lists the functions ("methods") that are part 
                                                   # of the object.
             getsolve = getsolve)                  # If a function is not on the list it cannot be 
                                                   # accessed by cacheSolve  
        
}




## The function cacheSolve computes the inverse of the "special" matrix returned by makeCacheMatrix,
## stores (i.e., cache) it and then retruns it.
## If the inverse has already been calculated (i.e., matrix has not been changed), then the
## cacheSolve fetches the inverese that was stored in the cache earlier and returns it.



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
        
        
        data <- x$get()                             # gets the value that was entred to makeCacheMatrix 
                                                    # (i.e., gets the new matrix)  
        
        inv.m <- solve(data, ...)                   # computes the inverse for the new matrix 
        
        x$setsolve(inv.m)                           # stores the calculates inverse value in 'x' 
        
        inv.m                                       # returns the inverse to the code that called this function 
}