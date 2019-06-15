bbc <- function(i,j,...){
  res <- list(i=i,j=j,...)
  print(res)
  
}

bbc(i=1,j=2,c=3,d=4);

bbc(123,122, , , , , ,'bbc');

bbc(123,123,123);
bbc(123,123, ,123)

bbc(123,'',123,'1232');
bbc('','','',bbc='123','');
