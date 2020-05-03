library(png)
library(RCurl)
img <- readPNG(getURLContent("http://pages.stat.wisc.edu/~jgillett/305/3/Van_Gogh.png"))
img = readPNG(getURLContent("http://pages.stat.wisc.edu/~jgillett/305/3/Madison.png"))
red.vg <- vg[,,1]
green.vg <- vg[,,2]
blue.vg <- vg[,,3]

pad = function(X,k){
  pad.X <- matrix(0, dim(X)[1]+2*k, dim(X)[2]+2*k)
  pad.X[(k+1):(dim(X)[1]+k), (k+1):(dim(X)[2]+k)] <- X
  return(pad.X)
}
mean_filter = function(X,k){
  pad.X = pad(X,k)
  idx <- (k+1):(dim(X)[1]+k)
  idy <- (k+1):(dim(X)[2]+k)
  result = matrix(0 , nrow = nrow(X),ncol = ncol(X))
  for(i in idx){
    for(j in idy){
      result[i-k,j-k] <- mean(pad.X[(i-k):(i+k),(j-k):(j+k)])
    }
  }
  result
}
sd_filter = function(X,k){
  pad.X = pad(X,k)
  idx <- (k+1):(dim(X)[1]+k)
  idy <- (k+1):(dim(X)[2]+k)
  result = matrix(0 , nrow = nrow(X),ncol = ncol(X))
  for(i in idx){
    for(j in idy){
      result[i-k,j-k] <- sd(pad.X[(i-k):(i+k),(j-k):(j+k)])
    }
  }
  result
}
sd_filter(red.vg,2)


layout(matrix(1:3, ncol=3))
image(t(red.vg[nrow(red.vg):1,]), col = gray((1:12)/13), main="Red channel")
image(t(green.vg[nrow(green.vg):1,]), col = gray((1:12)/13), main="Green channel")
image(t(blue.vg[nrow(blue.vg):1,]), col = gray((1:12)/13), main="Blue channel")

array(0,dim = c(2,2,3))


edge_detection = function(img,k,thres){
  # Extracting different colour chanels:
  red   <- img[,,1]
  green <- img[,,2]
  blue  <- img[,,3]
  
  edge_img = array(0,dim = c(nrow(red),ncol(red),3))
  
  # Applying sd filter:
  red   <- sd_filter(red,k)
  green <- sd_filter(green,k)
  blue  <- sd_filter(blue,k)
  
  out.red <- out.green <- out.blue <- matrix(0,nrow = nrow(red),ncol = ncol(red))
  # Edge detection:
  ind_red   <- which(red > quantile(red,1-thres),TRUE)
  out.red[ind_red] <- 1
  ind_green <- which(green > quantile(green,1-thres),TRUE)
  out.green[ind_green] <- 1
  ind_blue   <- which(blue > quantile(blue,1-thres),TRUE)
  out.blue[ind_blue] <- 1
  
  edge_img[ , , 1] <- out.red
  edge_img[ , , 2] <- out.green
  edge_img[ , , 3] <- out.blue
  
  return(edge_img)
}

edge = edge_detection(img,1,0.1)
writePNG(edge,"sa.png")

layout(matrix(1:3, ncol=3))
image(t(edge[,,1][nrow(red.vg):1,]), col = gray((1:12)/13), main="Red channel")
image(t(edge[,,2][nrow(red.vg):1,]), col = gray((1:12)/13), main="Green channel")
image(t(edge[,,3][nrow(red.vg):1,]), col = gray((1:12)/13), main="Blue channel")


mean_filter_image = function(X,k,cl){
  temp = list()
  temp[['red']] = X[,,1]
  temp[['green']] = X[,,2]
  temp[['blue']] = X[,,3]
  assign("k", "k", envir = .GlobalEnv)
  snow::clusterExport(cl,list = ls())
  temp = pblapply(temp, function(x){mean_filter(x,k)},cl=cl)
  filtered_img = array(0,dim = c(nrow(X),ncol(X),3))
  filtered_img[,,1] = temp[['red']]
  filtered_img[,,2] = temp[['green']]
  filtered_img[,,3] = temp[['blue']]
  return(filtered_img)
}
mean_filter_image(X,1,cl)
