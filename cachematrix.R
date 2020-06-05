##As funções abaixo computam a inversa de uma matrix, colocam esse cache em uma lista,
## transforma a matrix em sua inversa. Através da segunda função, puxamos a inversa dessa matrix 


## Essa função transforma a matrix (argumento da função) em sua inversa e guarda em uma lista

makeCacheMatrix <- function(x = matrix()) {
      j <- NULL
      set <- function(y) {
            x <<- y
            j <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) j <<- inverse
      getInverse <- function() j
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Essa função retorna a matrix inversa de x, se a matrix inversa já estiver no cache, 
## ela usa a memória, caso contrário ela inverte e retorna a inversa da matrix.

cacheSolve <- function(x, ...) {
        j <- x$getInverse()
        if (!is.null(j)){
              message("colhendo o cache")
              return(j)
        }
        data <- x$get()
        j <- solve(data,...)
        x$setInverse(j)
        j
}


