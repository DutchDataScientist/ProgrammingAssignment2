## Put comments here that give an overall description of what your
## functions do.
# The functions below are demonstrators of lexical scoping by means of 
# using the superassignment operator '<<-' to create caching variables
# outside the scope of function they are used in for other functions to use.
# The functions are also demonstrators for scoping rules by means of
# defining functions within functions and passing functions as arguments and
# returning functions as values. It is showcasing that functions in R are
# handled as what is generally called first-class objects in programming.
#
# Below are two functions that are used to create a special object
# which is a list of functions as return value that caches a
# matrix and an inverted matrix as its variable values inside these
# functions. 
# This is preserving the variable state within the function object
# while exposing the variable values outside their function scope by 
# making them accessible through the superassignment operator.
# Caching variables in a function for outside access by other functions for
# reuse is usefull when recalculating the value really costs a lot of time.

## Write a short comment describing this function
# `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
# 
# The first function, `makeCacheMatrix` creates a special "vector", which is
# really a list containing a function to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverted matrix
# 4.  get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        # Create an undefined variable
        m <- NULL
        
        # Define 4 internal functions.
        # This is like defining private internal methods of an object

        set <- function(y) {
                # Create the caching variables x for the matrix value and 
                # m for the inverted matrix value.
                # Make x and m accessible from an outside (parent) environment
                # then this 'child'environment in which they are defined by
                # using the superassignment operator.
                # This scoping is like creating 'global' variables with 
                # access for other functions or it's like a 'free' variable 
                # idea from another environment.
                # Set the value of a matrix.
                #
                # Remember: here m is only intialised. The cacheSolve function
                # is needed to actually fill the variable with the inverted
                # matrix value!
                # See the Testcase below the function definitions!
                x <<- y
                m <<- NULL
        }
        
        # Make the function to recall the value from the
        # 'global' variable x. Get the value of a matrix.
        get <- function() x
        
        # Make the function to superassign the inverted matrix value 
        # to the 'global' variable m.
        # Set the value of the inverted matrix.
        setmatrix <- function(cacheSolve) m <<- cacheSolve

        # Make the function to recall the inverted matrix value
        # from the 'global' variable m.
        # Get the value of the inverted matrix.
        getmatrix <- function() m
        
        # Create the return value of the makeCacheMatrix function
        # with as its return values the 4 internal functions and
        # give each function a named label for easy reference by name.
        # This is like making the private functions of an object public.
        # Set the value of the vector.
        list(set = set, get = get,
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
}


## Write a short comment describing this function
# `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
# 
# Conditions: 1) the matrix x that is used is assumed to always be an
# invertible matrix. 2) use solve(x) to calculate the inverse matrix.
# 
# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse matrix has already been calculated. 
# If so, it get's the inverse matrix from the cache and skips the computation. Otherwise, it calculates the mean of
# Otherwise the value of the inverse matrix in the cache is set via 
# the 'setmatrix' function.
#
# Remember this function needs to be called seperately from makeCacheMatrix
# to actually fill the intialised m variable with the inverted matrix value!
# See the Testcase below the function definitions!


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the matrix value from the cache variable
        m <- x$getmatrix()
        
        # if the cache variable m has a value then this inverse matrix
        # variable is returned and this function stops here.
        if(!is.null(m)) {
                message("Getting cached matrix!")
                return(m)
        }
        
        # The matrix variable for the inverted matrix was undefined
        # so now calculate the inverse matrix m from the matrix x.
        # Get matrix x and put it in the internal data variable
        data <- x$get()
        
        # Calculate the inverse matrix
        m <- solve(data, ...)
        
        # Set the cache variable m
        x$setmatrix(m)
        
        # Return the inverted matrix value m
        m
}

# - - - - - - - - - - - - - - - - - - - - - - - - -
# Testcase 
# - - - - - - - - - - - - - - - - - - - - - - - - -
# Testcase design: 
# 1) create an invertable matrix mtx
# 2) call makeCacheMatrix to store mtx in x and initialise m (NULL)
# 3) call v$get() to prove x contains mtx
# 4) call v$getmatrix() to prove m is still NULL
# 5) call cacheSolve to calculate and store the inverse matrix in m
# 6) call v$getmatrix() to prove m now contains the inverted x matrix
# 7) call cacheSolve again to prove the cache variable m is accessible
#
# Testcase output:
# > mtx <- matrix(1:4, 2, 2)
# > mtx
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > v <- makeCacheMatrix(mtx)
# > v$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > v$getmatrix()
# NULL
# > cacheSolve(v)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(v)
# Getting cached matrix!
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
# 