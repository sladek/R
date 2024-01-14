library(parallelly)
library(stringr)
#library(logger)

# Start up a parallel cluster
cluster1 <- rep('192.168.2.9', times=8L)
cluster2 <- rep('192.168.2.14', times=4L)
#cluster3 <- rep('192.168.2.14', times=1L)
#cluster4 <- c(rep('192.168.2.9', times=8L), rep('192.168.2.14', times=4L))

cluster <- c(cluster1, cluster2)

cl <- parallelly::makeClusterPSOCK(cluster, user='sladekm',
      outfile='/tmp/mytest.log',
      default_packages=c("stringr", "stats", "logger"),
      verbose=TRUE)
print(cl)
# end of start of the cluster 

# Test function than will run as worker on the node
# Parameters:
# nRepeate - defines number of repeates the function FUN will execute
# parameter - parameter of the function FUN
# FUN - name of the function to be called, e.g. sqrt, log, .....
myTestFunction <- function(nRepeate, parameter, FUN) {
  res <- parameter
  for (i in 1:nRepeate) {
    res <- FUN(parameter)
  }
#  as.character(cat(parameter,'\n', sep = ''))
}

source('bindToEnv.R') # Download from: http://winvector.github.io/Parallel/bindToEnv.R
# We will build the single argument function we are going to pass to parallel
mkWorker <- function() {
# Bind all the necessary variables and function to the environment so that remote node can see it
    bindToEnv(objNames=c('parallelInput', 'myTestFunction', 'logElapsedTime'))
# We will return function that will execute myTestFunction() on the node
  function(parallelInput){
    currentTs <- Sys.time()
    myTestFunction(10, parallelInput, log)
#    logElapsedTime(currentTs)
  }
}

# Calculate and log the elapsed time from previous timestamp
logElapsedTime <- function(previousTimestamp) {
  elapsed <- as.numeric(difftime(time1 = Sys.time(), time2 = previousTimestamp, units = "secs"))
  pid <- Sys.getpid()
  log_message <- cat( '[PID=',pid,  ', hostname: ', system('hostname', intern = T) , ', elapsed time: ', elapsed, ' seconds]\n',  sep='')
}

myTestFunction(4, 100, sqrt)

currentTs <- Sys.time()
# Parallel execution
parallelInput <- seq(1, 12000000)
models <- parallel::parLapply(cl, parallelInput, mkWorker())
# End of parallel execution

logElapsedTime(currentTs)

# stopping claster
parallel::stopCluster(cl)
