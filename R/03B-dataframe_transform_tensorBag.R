#  3构建tensorBag信息







#' 将数据框进行标准化处理
#'
#' @param data_frame 数据框类型
#' @param value_var 数值列
#' @param fillValue 插补值
#' @param autoSort 自动排序
#'
#' @return 返回值
#' @export
#'
#' @examples 
#' tf_normalized();
tf_normalized <- function(data_frame,value_var,fillValue = 0,autoSort=TRUE,showFilled=FLASE) {
  #获取字段列表
  fieldName_categories <-tf_fieldNames(data_frame,value_var)$category;
  fieldName_value <- tf_fieldNames(data_frame,value_var)$value;
  #处理类别数据
  data_categories <-data_frame[,fieldName_categories];
  data_categories <- lapply(data_categories,as.character);
  data_categories <- as.data.frame(data_categories,stringsAsFactors=FALSE);
  #处理张量阶的信息
  edgeNames<-data_categories %>% lapply(unique);
  #处理张量名称的信息
  edges <- edgeNames %>% lapply(length) %>% unlist();
  #View(data_categories);
  #处理张量容量信息
  full_capacity <- edges %>% prod();
  #处理张量实际容量
  
  edge_len <- length(edges);
  real_capacity <-nrow(data_categories);
  #是否需要插补
  is_fill <- full_capacity != real_capacity
  #处理数值数据
  data_value <- data_frame[,fieldName_value,drop=FALSE];
  data_value <-lapply(data_value,as.numeric);
  data_value <-as.data.frame(data_value,stringsAsFactors = FALSE);
  data_all <-cbind(data_categories,data_value);
  #兼容之前的数据
  res1 <- list(category=data_categories,value=data_value,
               all=data_all
  )
  
  if (is_fill == TRUE){
    
    data_filled_diff <-tf_fillData(res1,fillValue = fillValue,autoSort = autoSort,showFilled = TRUE)
    
    data_filled_all <-tf_fillData(res1,fillValue = fillValue,autoSort = autoSort,showFilled = FALSE)
    data_value <- as.numeric(data_filled_all[,fieldName_value,drop=TRUE]);
    edgeNames<-data_filled_all[,-ncol(data_filled_all)] %>% lapply(unique);
    edges <-edgeNames %>% lapply(length) %>% unlist;
    
    res2 <- list(category=data_categories,value=data_value,
                 all=data_all,
                 edgeNames=edgeNames,
                 edges=edges,
                 full_capacity=full_capacity,
                 real_capacity=real_capacity,
                 is_fill=is_fill,
                 data_filled_all=data_filled_all,
                 data_filled_diff=data_filled_diff)
    
    
  }else{
    
    res2_sorted_expr <-tf_autoSort_genExp(res_name = 'data_all',res_to_name = 'data_filled_all',col_count = edge_len)
    eval(res2_sorted_expr)
    data_value <- as.numeric(data_filled_all[,fieldName_value,drop=TRUE]);
    edgeNames<-data_filled_all[,-ncol(data_filled_all)] %>% lapply(unique);
    edges <-edgeNames %>% lapply(length) %>% unlist;
    res2 <- list(category=data_categories,value=data_value,
                 all=data_all,
                 edgeNames=edgeNames,
                 edges=edges,
                 full_capacity=full_capacity,
                 real_capacity=real_capacity,
                 is_fill=is_fill,
                 data_filled_all=data_filled_all)
    
  }
  
  return(res2)
}









#' 获取标准化后tf的实际容量
#'
#' @param tf_normalized 标准化后的tf数据
#'
#' @return 返回值
#' @export
#'
#' @examples tf_capacity_read();
tf_capacity_real <- function(tf_normalized){
  res <- dim(tf_normalized$category)[1];
  return(res)
}






#' 用于生成tf排序表达式
#'
#' @param res_name  原始结果集名称
#' @param col_count 排序字段总数
#' @param res_to_name 目标字段名称
#'
#' @return 返回值
#'
#' @examples tf_autoSort_genExp();
tf_autoSort_genExp <- function(res_name='res',res_to_name='res2',col_count){
  sort_asign <-paste(res_to_name,' <- ')
  sort_pefix <-paste(res_name,'[order(',sep='')
  sort_body <-paste(res_name,'[,',1:col_count,']',sep="",collapse = ',');
  sort_suffix <-"),];"
  res <- paste(sort_asign,sort_pefix,sort_body,sort_suffix,sep="");
  expr <-parse(text = res);
  return(expr)
  
}


#' 打印数据
#'
#' @param object 显示对照
#'
#' @return 返回值
#' @export
#'
#' @examples print();
print.tensorFrameFilled <- function(object){
  res <- object
  class(res) <- 'data.frame'
  cat('an object of  class: tensorFrameFilled\n')
  cat('powered by reshapedata.com \n')
  print(res);
}

#' 提供对tensorFrame对象的打印支持
#'
#' @param object 数据框对象
#'
#' @return 返回值 
#' @export
#'
#' @examples print();
print.tensorFrame <- function(object){
  res <- object$data
  class(res) <- 'data.frame'
  cat('an object of  class: tensorFrame\n')
  cat('powered by reshapedata.com \n')
  print(res);
} 
#' 处理数据框数据，根据不同情况进行判断
#'
#' @param data_frame 数据框，支持tbl,dataframe,tensorFrameFilled三种类型
#' @param value_var 数据所在列,默认为Fvalue
#' @param fillValue 填充值,默认为0
#'
#' @return 返回值
#' @export
#'
#' @examples 
#' data('tensorFrameExample')
#' res <- tensorFrame(tensorFrameExample);
#' res;
tensorFrame <- function(data_frame,value_var='Fvalue',fillValue=0,autoSort=TRUE,showFilled=FALSE) {
  is_tbl <- 'tbl' %in% class(data_frame)
  if(is_tbl){
    data_frame <- as.data.frame(data_frame,stringsAsFactors=F)
  }
  is_data_frame <- class(data_frame) == 'data.frame'
  if(is_data_frame){
    res <-tf_normalized(data_frame = data_frame,
                        value_var = value_var,
                        fillValue=fillValue,
                        autoSort = autoSort,
                        showFilled = showFilled)
    res <-list(
      edgeCount=as.integer(res$full_capacity),
      edges = res$edges,
      edgeNames=res$edgeNames,
      data=res$data_filled_all,
      value=as.numeric(res$value)
    )
    class(res) <-'tensorFrame'
    
  }else{
    stop('数据类型应该为tbl或data.frame,请重新检查',call. = F)
  }
  return(res)
}

#' 将tensorFrame数据转化为tensorBag
#'
#' @param tensorFrame 数据框
#'
#' @return 返回值 
#' @export
#'
#' @examples 
#' data('tensorFrameExample')
#' res <- tensorFrame(tensorFrameExample);
#' res2 <- tensorBag(res);
#' res2;
tensorBag.tensorFrame <- function(tensorFrame) {
  edgeCount <- tensorFrame$edgeCount;
  #注意张量的装载顺序与data.frame还是相反的
  #因此需要使用rev调换顺序
  edges <- rev(tensorFrame$edges);
  edgeNames <- rev(tensorFrame$edgeNames);
  data <-  tensorFrame$value;
  data <-array(data,edges,dimnames = edgeNames)
  res1 <- new('tensorBag'); 
  res2 <- initialize(res1,edgeCount = edgeCount,
                     edges=edges,
                     edgeNames=edgeNames,
                     data=data)
  return(res2)
  
}

#' 将数据框直接转化为tensorBag
#'
#' @param data data_frame数据
#'
#' @return 返回值
#' @export
#'
#' @examples 
#' data('tensorFrameExample')
#' res <- tensorBag(tensorFrameExample);
#' res;
tensorBag.data.frame <- function (data){
  res <- tensorFrame(data);
  res2 <- tensorBag(res);
  return(res2);
}
