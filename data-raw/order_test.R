 library(readxl)
 test <- read_excel("data-raw/test_full.xlsx", 
                              sheet = "data")
 test <- as.data.frame(test)
 bb <- df_orderData('test',c('gsmc','year','quarter'),c(T,T,F));
 View(bb);

 
df_orderData <- function(srcData='data',order_by,is_asc=order_d){
  order_d <- rep(T,length(order_by));
  #order_by <- names(test)[1:4];
  order_by2 <- paste(order_by,collapse = ",");
  #处理排序字段逻辑
  is_asc2 <- !is_asc;
  is_asc2 <- paste(is_asc2,collapse = ',');
  #
  newData <-'res_ordered';
  expr <-paste(newData," <- ",
               srcData,"[with(",srcData,
               ",order(",order_by2,
               ",decreasing = c(",is_asc2,"),method='radix')),]",
               sep = "")
  expr_pared <-parse(text = expr)
  eval(expr = expr_pared)
  return(res_ordered);

}
bb <- df_orderData('test',c('gsmc','year','quarter','bbxm'));
bb;




expression(res_ordered <- test[with(test,order(gsmc,year,decreasing = c(TRUE,FALSE))),])


res_ordered <- test[order(test$gsmc,test$quarter,decreasing = c(F,F),method = 'radix'),]
View(res_ordered);
