hello <- function() {
  print("Hello, world!")
}
#行样本，列特征，对样本聚类
specClust<-function(data,type="expr",k=10){
  if(type=="expr"){
    simData<-getSimData(data)
  }else{
    simData<-data
  }
  W<-getWbyKNN(simData,k)
  L<-getL2(W)
  U<-getEigen2(L)
  result<-kmeans(U,ncol(U))
  return(result)
}
#可以优化对称三角形，先放着
getSimData<-function(data,segma=1){
  data<-scale(data,center=T,scale = T)
  distMatrix<-as.matrix(dist(data)) #dist求得是行之间的距离，即样本距离
  distMatrix<-exp(-distMatrix^2/(2*segma^2)) #高斯核函数
  return(distMatrix)
}
getWbyKNN<-function(simData,k=10){
  if(dim(simData)[1]!=dim(simData)[2])
    stop("simData应该是方阵!")
  #将k项之后的置0
  simData<-as.matrix(simData)
  W<-apply(simData,2,function(x){
    num_k<-sort(x,decreasing = T)[k] #从大到小排序，第k个的值
    x[x<num_k]=0 #稀疏化
    return(x)
  })
  diag(W)<-0 #对角线置0
  #构造对称矩阵
  W<-(W+t(W))/2
  return(W)
}
#正则拉普拉斯
getL1<-function(W){
  D<-diag(rowSums(W))
  D2<-diag(rowSums(W)^-.5)
  L<-D-W
  #正则化L
  L<-D2%*%L%*%D2
  return(L)
}
#随机游走拉普拉斯
getL2<-function(W){
  D<-diag(rowSums(W))
  D2<-diag(rowSums(W)^-1)
  L<-D-W
  L<-D2%*%L
  return(L)
}
getEigen1<-function(L,k){
  U<-getEigen2(L,k)
  #单位化
  U<-apply(U,1,function(x){x<-x/(norm(as.matrix(x), "F"))})
  U<-t(U)
}
getEigen2<-function(L){
  L.eigen<-eigen(L)
  L.vectors<-L.eigen$vectors
  L.values<-L.eigen$values
  #绘制特征值帮助选则
  plot(c(1:length(L.values)),L.values)
  k<-0
  while(k<=0|k>ncol(L.vectors)|!is.integer(k)){
    k<-readline(paste("输入划分的簇数,最大不超过 ",length(L.values)," :"))
    k<-as.integer(k)
  }
  U<-L.vectors[,(ncol(L.vectors)-k+1):ncol(L.vectors)]
}
#展示2维
showClust<-function(data,cluster){
  maxX<-max(data[,1])
  minX<-min(data[,1])
  maxY<-max(data[,2])
  minY<-min(data[,2])
  colors<-c("green","blue","yellow","grey","pink","purple")
  clusts<-unique(cluster)
  datas<-lapply(clusts,function(x){data2<-data[which(cluster==x),]})
  plot(datas[[1]],col="red",xlim = c(minX, maxX),ylim = c(minY, maxY))
  if(length(datas)>1){
    for(i in 2:length(datas)){
      points(datas[[i]],col=colors[i-1])
    }
  }

}
chooseK<-function(data,type=1){
  if(type==1){
    out<-hclust(dist(data),method = "average")
  }else{
    out<-hclust(as.dist(data),method = "average")
  }
  plot(out)
}
