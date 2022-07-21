#library(HadamardR)
how.many = function(x)    length(which(x)) #x is logical
is.normalized = function(H) (all(H[1,] == 1) & all(H[,1] == 1))
normalizeH = function(H)
{
  if(is.normalized(H)) return(H)
  num1 = apply(H, 1, function(x) how.many(x == -1))
  if(how.many(num1 == min(num1)) > 1) row.num = which.max(num1) else row.num = which.min(num1)
  sumHrownum  = sum(H[row.num,])
  if(sumHrownum != nrow(H)) 
  {  
    if(sum(H[row.num,])>0) coln = which(H[row.num,] == -1) else coln = which(H[row.num,] == 1)
    H[,coln] = -H[,coln]
    num1 = apply(H, 1, function(x) how.many(x == -1))
  }  
  if(num1[row.num] == nrow(H) | num1[row.num] == 0) 
  {
    success = 1
    tmp = H[row.num,]
    if(sum(tmp) == -nrow(H)) tmp = -tmp
    H[row.num,] = H[1,]
    H[1,] = tmp
  }  
  num1 = apply(H, 2, function(x) how.many(x == -1))
  if(how.many(num1 == min(num1)) > 1) col.num = which.max(num1) else col.num = which.min(num1)
  if(sum(H[,col.num])>0) rown = which(H[,col.num] == -1) else rown = which(H[,col.num] == 1)
  H[rown,] = -H[rown,]
  num1 = apply(H, 2, function(x) how.many(x == -1))
  if(num1[col.num] == nrow(H) | num1[col.num] == 0) 
  {
    success = 1
    tmp = H[,col.num]
    H[,col.num] = H[,1]
    H[,1] = tmp
  }  
  if(all(H[,1]== -1) & all(H[1,]== -1)) H = -H
  if(is.normalized(H)) return(H) else return(0)
}