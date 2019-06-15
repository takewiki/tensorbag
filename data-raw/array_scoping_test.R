dim_names<-list(R=c('r1','r2','r3'),
                C=c('c1','c2','c3'),
                B=c('b1','b2','b3'),
                S=c('s1','s2','s3'))

myarray <- array(data = 1:81,dim = c(3,3,3,3),dimnames = dim_names);
myarray;
#
myarray[ 'r1','c2' , ,'s1',drop=F];
myarray[ 'r1','c2' , ,,drop=T];


mydata2 <- aperm(myarray,c('B','S','C','R'));
mydata2;
