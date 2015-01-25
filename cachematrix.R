## This is the makeCacheMatrix function. Details below:

makeCacheMatrix <- function(x = matrix()) 
{
	# create a special "matrix" object that can cache its inverse
	m <- NULL					# create a cache
	#print(environment())
  	#evn <- environment()

	set <- function(y)				# set the value of the special "matrix" object
	{
      		x <<- y
		m <<- NULL
		message("just set the function")
	}

	get <- function() 				# get the value of the special "matrix" object
	{
		x				
	}

	setmatrix <- function(matrix) 
	{
		m <<- matrix$get()				# put the special "matrix" object inside the cache object 'm'
		message("just set the value of m")	# message notifying the setting of the value of the special "matrix" object
	}						
								

	getmatrix <- function() 
	{
		m					# get the value of the special "matrix" object from the cache 'm'
		message("just got the value")		# message notifying the getting of the value of the special "matrix" object
	}

	list(set = set, get = get,			# list of functions
		setmatrix = setmatrix ,
		getmatrix = getmatrix )
}




## This is the cacheSolve function. Details below:

cacheSolve <- function(x, ...) 
{
	#compute the inverse of the special "matrix" object returned by makeCacheMatrix
	#if the inverse has already been calculated and if the matrix has not changed
	#then retrieve the inverse from the cache
     	
	m <- x$get()		# set value of m

	inv <- getinv(x)		# set value of inverse of x

	if(!is.null(m)) 					# if m is not null
	{
		message("value detected")				# display message saying that there is a valid value for m
		if (m == inv)						# if m is equal to the inverse, aka if the inverse has been calculated already
		{
			message("inverse already calculated")			# then display a message saying the inverse already exists
			x							# display x
		}
		else							# if the inverse has not been calculated already
		{
			message("here is the calculated inverse")		# then display a message introducing the calculated inverse	
			solve(m)						# calculate and display the inverse of m
		}
	}
	else							# else (if m is null)
	{
		message("no value detected")				# display message saying there are no valid values for m
	}
}




# This is the getinv function, used in the cacheSolve function. Details below:
getinv <- function(x,...)
{
	m <- x$get()		# set value of m
	solve(m)		# get the inverse of m
}

