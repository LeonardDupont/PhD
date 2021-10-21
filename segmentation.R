rm(list=ls())
setwd("./Documents/PhD/PAPEL_related/PAPEL_2020/Vues_satellitaires")
library(imager)
library(parallelDist)
library(class)
library(gmodels)
library(LICORS)
library(OpenImageR)
library(spatstat)
library(Rvision)
library(leaflet)
library(EBImage)
library(opencv)

### PART 1 : creating CIElab images of all lakes ###

Zones = c('Ayous','Bassies','Neouvielle','Camporells') 
z = length(Zones)
for (zz in 1:z){
  if(zz==1){gothere = paste("./",Zones[zz],sep="")}
  else{gothere = paste('../',Zones[zz],sep="")}
  setwd(gothere)
  
  a = list.files()
  n = length(a)
  
  for (i in 1:n){
    im = load.image(a[i])
    x = dim(im)[1]
    y = dim(im)[2]
    uwpd = matrix(0,x*y,3)
    for (k in 1:3){
      uwpd[,k] = as.vector(im[,,1,k])
    }
    
    # now we convert the image to the CIE color space
    im2 = convertColor(uwpd,'sRGB', 'Lab',to.ref.white='D65')
    
    #creating the image again
    wpd = array(rep(0,x*y*3),c(x,y,1,3))
    for (k in 1:3){
      wp = matrix(im2[,k],nrow=x, ncol=y)
      wpd[,,1,k] = wp
    }
    xx = as.cimg(wpd)
    
    if (i==1){
      dir.create(path="./CIElab_images")
    }
    
    lake = strsplit(a[i],'\\.')[[1]]
    lake = lake[1]
    name = paste("./CIElab_images/",lake,"_CIElab.jpeg", sep='')
    imager::save.image(xx,name)
  }
 
  rm(uw)
  rm(uwpd)
  rm(wp)
  rm(wpd)

  
  ### PART 2 : trying to cluster lake pixels to extract their area ###
  


  
  #try 2 : starting from k-means all alone
  
  xx1 = xx[,,1,1] #the l channel from l*a*b
  #xds1 = down_sample_image(xx1,5,gaussian_blur = TRUE, gauss_sigma = 5)
  #image(xds1)
  
  
  xx3 = xx[,,1,3] # the b channel from l*a*b
  #xds3 = down_sample_image(xx3,5,gaussian_blur = TRUE, gauss_sigma = 5)
  #image(xds3)
  
  
  imk = im[,,1,3] #the blue channel from RGB
  #idsk = down_sample_image(imk,5,gaussian_blur = TRUE, gauss_sigma = 5)
  #image(idsk)

  
  
  imk = as.vector(imk)
  xx3 = as.vector(xx3)
  xx1 = as.vector(xx1)
  xkm = array(rep(0,x*y*3),c(x*y,3))
  xkm[,1] = xx1
  xkm[,2] = xx3
  xkm[,3] = imk
  

  
  n = 4 #number of kmeans clusters
 # kk = kmeanspp(xkm, k=n, start='random')  #kmeans++ 
  kk = kmeans(xkm, n)
  kkmat=matrix(kk$cluster,nrow=x,ncol=y)
  #image(matrix(kk$cluster,nrow=x,ncol=y))
  
  #kkmat2 = down_sample_image(kkmat, 10, gaussian_blur=TRUE, gauss_sigma=50, range_gauss=10)
  #image(kkmat2)
  
  
  clustcol = colorFactor("grayscale", 1:n)
  blobmat = array(rep(0,x*y*3),c(x,y,3))
  for (k in 1:n){
    clv = (kkmat==k)
    ccl = col2rgb(clustcol(k))
    for (r in 1:x){
      for (c in 1:y){
        if (clv[r,c]){
          for (i in 1:3){
            blobmat[r,c,i] = ccl[i]
          }
        }
      }
    }
  }
  
  graphics::image(as.cimg(blobmat))
  blobmat = Rvision::image(blobmat, colorspace='rgb')
  blbs = simpleBlobDetector(blobmat, min_threshold = 5000, min_dist_between_blobs = 100, filter_by_area = TRUE,
                     min_area = 1e4, filter_by_inertia = FALSE, filter_by_convexity = FALSE)
  
  
  e
  
  distmat = array(rep(1000,x*y*(n+2)),c(x,y,(n+2)))
  for (r in 1:x){
    for (c in 1:y){
      distmat[r,c,n+1] = r/(x)
      distmat[r,c,n+2] = c/(y)
    }
  }
  for (cl in 1:n){
    clone = (kkmat==cl)
    clone[clone==FALSE] = 0
    clone[clone==TRUE] = 2
    distmat[,,cl] = clone
  }
  
  rm(clone)
  dim(distmat) = c(x*y,(n+2))
  
  #unfortunately, this is too big for regular clustering
  sspld = sample(1:dim(distmat)[1], 3e4)
  subsampled <- distmat[sspld,]
  Dmat = dist(subsampled,method="manhattan")
  hc = hclust(Dmat,method='complete')
  #plot(hc)
  
  kl=3 #number of groups from the tree
  brchs = cutree(hc,kl)
  
  #clearing a bit
  rm(hc)
  rm(kk)
  rm(kkk)
  rm(kkmat)
  rm(wherearethey)
  rm(wp)
  rm(Dmat)
  rm(im)
  rm(xxx)
  rm(wpd)
  
  testmat = array(rep(0,x*y),c(x,y))
  testmat = as.vector(testmat)
  
  #visualise where the 1Ok points are located within the image
  #testmat[sspld] = 1 
  #testmat = matrix(testmat,x,y) 
  #image(testmat)
  
  
  for (k in 1:length(sspld)){
    testmat[sspld[k]] = brchs[k]
  }
  testmat = matrix(testmat,x,y) 
  image(testmat)
  
  
  # building a knn predictor to extrapolate the subsampled classification
  knndata= array(rep(0,length(sspld)*3),c(length(sspld),3))
  ctr = 1
  for (r in 1:x){
    for (c in 1:y){
      vl = testmat[r,c]
      if (vl!=0){
        knndata[ctr,1] = vl
        knndata[ctr,2] = r
        knndata[ctr,3] = c
        ctr = ctr + 1
      }
    }
  }
  
  
  knntrain = knndata[1:2.5e4,2:3]
  knntest = knndata[25001:3e4,2:3]
  
  
  model_test =  knn(train = knntrain, test = knntest, cl=knndata[1:2.5e4,1],k=10)
  test_res = CrossTable(x = knndata[25001:3e4,1], y=model_test,  digits=3, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                        prop.t=TRUE, prop.chisq=TRUE, chisq = FALSE, fisher=FALSE, mcnemar=FALSE,
                        resid=FALSE, sresid=FALSE, asresid=FALSE,
                        missing.include=FALSE,
                        format=c("SAS","SPSS"), dnn = NULL)
  
  
  
  Nmissing = length(testmat) - length(sspld)
  missing_pos = (testmat==0)
  missing_px = array(rep(0,Nmissing*2),c(Nmissing,2))
  
  ctr=1
  for (r in 1:x){
    for (c in 1:y){
      if (missing_pos[r,c]){
        missing_px[ctr,1] = r
        missing_px[ctr,2] = c
        ctr = ctr + 1
      }
    }
  }
  
  titi =  knn(train = knndata[,2:3], test = missing_px, cl=knndata[,1],k=10)
  

  for (k in 1:length(missing_px)){
    r = missing_px[k,1]
    c = missing_px[k,2]
    testmat[r,c] = titi[k]
  }
  
  missing_pos = (testmat==0)
  
  testmat = matrix(testmat,x,y)
  image(testmat)
  
  
  # PART 3 : blob detection 
  
  #this will be removed
  im = load.image("Gentau_knn.png")
  plot(im)
  simpleBlobDetector(as.im(im))
  
  }
  


 
  
  
  
imagej_path = paste("/Users/leonarddupont/Documents/PhD/PAPEL_related/PAPEL_2020/Vues_satellitaires/",
                    Zones[zz],'/CIElab_images/', sep='')
c
system(paste("./Applications/ImageJ/ImageJ.app/Contents/MacOS/JavaApplicationStub -batch 
/Users/leonarddupont/Documents/PhD/PAPEL_related/PAPEL_2020/Vues_satellitaires/lake_ROI.ijm",imagej_path, sep=" "))  
  
  




