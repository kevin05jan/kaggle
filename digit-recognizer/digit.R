###!!!!             https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/

#### naive bayes classifier // library(e1071);  m = naiveBayes(y~., data)

### https://www.mathsisfun.com/data/correlation.html

### https://rosettacode.org/wiki/Zhang-Suen_thinning_algorithm
### http://www.rupj.net/portfolio/docs/skeletonization.pdf
### https://github.com/matiasb/biOps/tree/master/man

library(squash)
library(kernlab)
library(dplyr)
library(EBImage)
#library(spatstat)
#library(hmeasure)










absDiff <- function(matrix1,matrix2)
{
  r <- nrow(matrix1)
  c <- ncol(matrix1)
  destMatrix <- matrix1
  for(r in 0:r-1)
    {
      for(c in 0:c-1)
        {
          destMatrix[r,c] <- abs(matrix1[r,c]-matrix1[r,c])
        }
    }
  return(destMatrix)
}

countNonZero <- function(inputMatrix)
{
  return(length(inputMatrix[inputMatrix > 0]))
}

thinningIteration <- function(imageMatrix, iter)
{
  imageInput <- imageMatrix
  r <- nrow(imageInput) - 1
  c <- ncol(imageInput) - 1
  for(i in 2:r)
    {
      for(j in 2:c)
        {
          p2 <- imageInput[i-1, j]
          p3 <- imageInput[i-1, j+1]
          p4 <- imageInput[i, j+1]
          p5 <- imageInput[i+1, j+1]
          p6 <- imageInput[i+1, j]
          p7 <- imageInput[i+1, j-1]
          p8 <- imageInput[i, j-1]
          p9 <- imageInput[i-1, j-1]
          A  <- (p2 == 0 && p3 == 1) + (p3 == 0 && p4 == 1) + 
            (p4 == 0 && p5 == 1) + (p5 == 0 && p6 == 1) + 
              (p6 == 0 && p7 == 1) + (p7 == 0 && p8 == 1) +
                (p8 == 0 && p9 == 1) + (p9 == 0 && p2 == 1)
          B  <- p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9
          if(iter == 0){
            m1 <- (p2 * p4 * p6)
            m2 <- (p4 * p6 * p8)
          }
          else {
            m1 <- (p2 * p4 * p8)
            m2 <- (p2 * p6 * p8)
          }
          if (A == 1 && (B >= 2 && B <= 6) && m1 == 0 && m2 == 0)
            {
              imageInput[i,j] <- 0
            }
        }
    }
  return(imageInput)
}

thinImage <- function(imageMatrix)
{
  im <- imageMatrix
  prev <- im
  repeat {
    im <- thinningIteration(im, 0)
    im <- thinningIteration(im, 1)
    diff <- absDiff(im, prev)
    prev <- im
    if(countNonZero(diff) <= 0)
      {
        break
      }
  } 
  return(im)
}


thinImage(imageData(as.Image(.[1000, -1])))

##-------
df  = read.table("train.csv.part", header=T, sep=",") %>% slice(2000)

## rescale values from 0 to 1 use:   scale(x, center=0, scale=256)
## montage i-*png -tile 10x10 -geometry +2+2 out.jpg
## filter(., label == 9) %>% head(100) %>% mutate(f=1:n()) %>% tidyr::gather(k,v,-c(f,label)) %>% group_by(f) %>% mutate(vv=scale(v, center=0, scale=256)) %>% do({ ff=sprintf("i9-%02d.png", .$f); savemat(t(matrix(.$vv,byrow=F, nrow=28)), filename=ff); data.frame(x=ff) })


## subsample
. = mutate(df, label=as.factor(label), f=1:n()) %>% tidyr::gather(k, v, -c(label, f)) %>%  mutate(v=scale(v, center=0, scale=256), k=as.integer(gsub("pixel", "", k))) %>% tidyr::spread(k,v) %>% select(-f)
n = nrow(.)
ii = sample(n, 100)
x = as.matrix(.[ii, -1])
### class as factor!!!
y = as.vector(.[ii, 1])

## ksvm(kernel='rbfdot'
## ksvm(kernel=rbfdot()
## ksvm(kernel='matrix', K  .... K = kernelMatrix(f(), x).....f = function(){ f=function(x,y){crossprod(x,y)}; class(f) = 'kernel'; k }
m = ksvm(x,y, kernel='rbfdot')
p = predict(m, as.matrix(.[-ii,-1]))

## confusion matrix
aa = table(actual=.[-ii, 1], predicted=p)
##hmeasure::misclassCounts(p, .[-ii, 1])


## accuracy
sum(p==.[-ii,1])/length(p)


 .a = mutate(., f=1:n()) %>% tidyr::gather(k,v, -c(label,f)) %>% mutate(v=scale(v, center=0, scale=256)) %>% group_by(f) %>% tidyr::spread(k,v) %>% arrange(f)



## kernel function
k = function(){
  f = function(x,y){ return(x+y) }
  class(f) <- "kernel"
  return(f)
}
m = ksvm(kernelMatrix(k(), x), kernel='matrix')
p = predict(m, as.matrix(.[-ii,-1]))
table(actual=.[-ii, 1], predicted=p)


##---
a = mutate(., f=1:n()) %>% group_by(f)  %>% do({ a = .[,grep("^[0-9]+", colnames(.))]; b = matrix(a, nrow=28); aa = as.Image(b) > 0;  data.frame(label=.$label, computeFeatures.shape(aa), computeFeatures.moment(aa)) }) %>% ungroup %>% select(-f) %>% as.data.frame
x = as.matrix(a[ii, -1])
y = as.vector(a[ii, 1])
m = ksvm(x,y, kernel='rbfdot')
x. = as.matrix(a[-ii,-1])
y. = as.vector(a[-ii, 1])
p = predict(m, x.)
bb = table(actual=y., predicted=p)








####---

## load
as.Image
readImage

## rgb to grey
channel(i, 'gray')

## threshold
i > .5 ## global threshold
thresh(i, 3, 3, .2) ## local threshold

## blob to obj
bwlabel(bw)
normalise(label)
colorLabels

## image analysis
computeFeatures.shape(label)



