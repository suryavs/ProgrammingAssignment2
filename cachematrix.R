
## Function to create special vector
makeCacheMatrix <-function(x=numeric()){
      
      ## Flusing cache for reset
      m<-NULL
      
      setm<-function(x1){
            
            ## To make sure the cache is not flushed if the matrix is identical
            if(identical(x,x1)){
                  x<<-x1
                  message("Don't flush cached value")
            }
            x<<-x1
            m<<-NULL
      }
      ## To retrieve the matrix
      getm<-function(){
            x
      }
      
      setmi<-function(mIn){
            m<<-mIn
      }
      
      getmi<-function(){
            m
      }
      
      list(setm=setm,getm=getm,setmi=setmi,getmi=getmi)
}

## Function to calculate the inverse of the matrix
cacheSolve<-function(x){
      m<-x$getmi()
      
      if(!is.null(m)){
            message("Returning cached value")
            return(m)
      }
      
      minverse<-x$getm()
      m<-solve(minverse)
      x$setmi(m)
      m
}
