library(parallelly)
library(stringr)

d <- iris # let "d" refer to one of R's built in data sets
vars <- c('Sepal.Length','Sepal.Width','Petal.Length')
yName <- 'Species'
yLevels <- sort(unique(as.character(d[[yName]])))
print(yLevels)
## [1] "setosa"     "versicolor" "virginica" 

# Start up a parallel cluster
cluster1 <- rep('192.168.2.9', times=8L)
cluster2 <- rep('192.168.2.14', times=4L)

cluster <- c(cluster1, cluster2)

cl <- parallelly::makeClusterPSOCK(cluster, user='sladekm',
                   outfile='/tmp/mytest.log',
                   default_packages=c("stringr", "stats"),
                   verbose=TRUE)

print(cl)

#clusterEvalQ(parallelCluster, stringr)

fitOneTargetModel <- function(yName,yLevel,vars,data) {
  currentTs <- Sys.time()
  formula <- paste('(',yName,'=="',yLevel,'") ~ ',
                   paste(vars,collapse=' + '),sep='')
  gl <- glm(as.formula(formula),family=binomial,data=data)
  elapsed <- Sys.time() - currentTs
  pid <- Sys.getpid()
  cat('$', str_pad(yLevel, 10, "right"), ' [elapsed time: ', elapsed , ', PID=',pid,  ', hostname: ', system('hostname', intern = T) , ']\n',  sep='')
  gl
}

source('bindToEnv.R') # Download from: http://winvector.github.io/Parallel/bindToEnv.R
# build the single argument function we are going to pass to parallel
mkWorker <- function() {
  bindToEnv(objNames=c('yName','vars','d','fitOneTargetModel'))
  function(yLevel) {
    fitOneTargetModel(yName,yLevel,vars,d)
  }
}


currentTs <- Sys.time()
models <- parallel::parLapply(cl, yLevels, mkWorker())
elapsed <- Sys.time() - currentTs

names(models) <- yLevels
print(models)
cat('Elapsed time is: ', elapsed, '\n')
# Shutdown cluster neatly
if(!is.null(cl)) {
  parallel::stopCluster(cl)
  cl <- c()
}


