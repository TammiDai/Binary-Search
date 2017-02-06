# Binary-Search
# Classic Binary Search Algorithm using R, object is to find whether a is in b:

binarysearch=function(a,b){
  left=0
  right=length(b)
  m=left+(right-left)%/%2
  while(a!=b[m] & left<=right){
    if(a<b[m]){
      right=m-1
    }else{
      left=m+1
    }
    m=left+(right-left)%/%2
  }
  if(left>right){
    return(-1)
  }
  return(m)
}
intersect=function(x,y){
  y=sort(y)
  l=numeric()
  for(i in 1:length(x)){
  j=binarysearch(x[i],y)
  if (j>0){
    l[i]=x[i]
  }
  }
  return(unique(l))
}
