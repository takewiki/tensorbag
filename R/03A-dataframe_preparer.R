# 目标
# 1处理edges与edgeNames信息
#    1.1.区分category与value列信息-----

#' 获取字段名称
#'
#' @param data_frame 数据框
#' @param value_var  数据列
#'
#' @return 返回值
#' @import tsdo
#' @export
#'
#' @examples tf_fieldNames();
tf_fieldNames <- function(data_frame,value_var){
  fieldName <-df_columnName(data_frame);
  if (length(value_var) != 1){
    stop('数值变量名必须为1列，请重新指定!',call. = F)
  }

  fieldName_categories <- setdiff(fieldName,value_var)
  res <- list(category=fieldName_categories,
              value=value_var);
  return(res);
}


#    1.2 针对上述进行类型检查------
#' 获取数据框的字段类型
#'
#' @param data_frame 数据框
#' @param value_var 数值型数据
#'
#' @return 返回值
#' @import tsdo
#' @export
#'
#' @examples tf_fieldTypes();
tf_fieldTypes <- function(data_frame,value_var){
  fieldTypes <-df_columnType(data_frame);
  fieldNames <-tf_fieldNames(data_frame,value_var);
  res <- list(category=fieldTypes[fieldNames$category],
              value=fieldTypes[fieldNames$value])
  return(res)
  
}



#' 判断tensorFrame的分类字段类型是否合法
#'
#' @param fieldtype_categories 分类字段类型
#'
#' @return 返回值
#'
#' @examples 
#' tf_categoryType_isInvalid(tf_fieldTypes(iris,'Sepal.Length')$category);
tf_categoryType_isInvalid <- function(fieldtype_categories){
  
  res <- !sum(as.integer(fieldtype_categories == 'character')) == length(fieldtype_categories)
  return(res)
}


#' 判断valueType是否合法
#'
#' @param fieldType_value 数据类型
#'
#' @return 返回值
#'
#' @examples
#' tf_valueType_isInvalid();
tf_valueType_isInvalid <- function(fieldType_value){
  
  res <- !sum(as.integer(fieldType_value == 'numeric')) == length(fieldType_value)
  return(res)
}

#    1.3 获取edgeNames信息------
#' 获取标准化tensorFrame的阶名称信息
#'
#' @param data_frame 数据框
#' @param value_var value值
#'
#' @return 返回一个命名列表NamedList,代表阶名称
#' @export
#'
#' @examples 
#' tf_normalized(tensorFrameExample,'Fvalue')->bb;
#' (tf_edgeNames(bb));
tf_edgeNames <- function(data_frame,value_var) {
  fieldCategory <- tf_fieldNames(data_frame,value_var)$category;
  data_categories <- data_frame[,fieldCategory,drop=FALSE];
  res<-data_categories %>% lapply(unique);
  return(res)
}

#    1.4 获取edges信息-------
#' 获取tf的阶信息
#'
#' @param data_frame 数据框
#' @param value_var  字段value
#'
#' @return 返回integer型的向量
#' @import magrittr
#' @export
#'
#' @examples tf_edges();
tf_edges <- function(data_frame,value_var) {
  tf_edgeNames <- tf_edgeNames(data_frame,value_var);
  res <- tf_edgeNames %>% lapply(length) %>% unlist();
  return(res)
}
#  2处理行信息
#    2.1检查行记录的完整性------
#' 获取tf理论总容量，与edgeNames有关,与实际数据无关
#'
#' @param tf_edgeNames  tf的阶维名称
#'
#' @return 返回integer的整数
#' @export
#'
#' @examples tf_capacity_nominal();
#' 
#' tf_normalized(tensorFrameExample,'Fvalue')->bb;
#' tf_capacity_nominal(tf_edgeNames(bb));
tf_capacity_full <- function(data_frame,value_var){
  edges <- tf_edges(data_frame,value_var);
  res <- edges %>% prod();
  return(res);
}

#' 获取标准化后tf的实际容量
#'
#' @param data_frame 数据框
#'
#' @return 返回值
#' @export
#'
#' @examples tf_capacity_read();
tf_capacity_real <- function(data_frame){
  nrow(data_frame)
}

#    2.2判断是否进行插入填补-----
#' 判断tensorFrame是否需要进行插补
#'
#' @param tf_normalized 标准化的数据框
#'
#' @return 返回值
#' @export
#'
#' @examples tf_isFillable();
#' tf_normalized(tensorFrameExample,'Fvalue')->bb;
#' tf_isFillable(bb);
tf_isFillable <- function(data_frame,value_var){
  
  
  tensor_Capacity_full <-tf_capacity_full(data_frame,value_var)
  tensor_Capacity_real <- tf_capacity_real(data_frame)
  res <-tensor_Capacity_full != tensor_Capacity_real
  return(res)
  
}

#    2.3针对数据进行插补-----
#    2.3.1插补前的准备工作------
#' 获取edgeName的eachVec向量，用于控制rep
#'
#' @param data_frame 数据框
#' @param value_var value字段类型
#'
#' @return 返回值
#'
#' @examples 
#' tf_normalized(tensorFrameExample,'Fvalue')->bb;
#' tf_edgeNames(bb)->edgeNames;
#' each_data <- tf_eachVec(edgeNames);
#' each_data;
tf_eachVec <- function(data_frame,value_var){
  edges <- tf_edges(data_frame,value_var);
  #获取辅助信息,阶的序号
  edge_index <- seq_along(edges);
  #阶的总长度
  edge_len <- length(edges);
  #处理each信息
  each_data <-c(edges[-1],1);
  names(each_data) <- names(edges);
  each_counter <-lapply(edge_index, function(i){
    prod(each_data[i:edge_len])
  })
  res <-unlist(each_counter);
  names(res) <- names(edges);
  return(res)
}

#' 获取timsVec信息,用于控制按向量长度进行整体复制
#'
#' @param data_frame 数据框
#' @param value_var value字段类型
#'
#' @return 返回值
#'
#' @examples 
#' tf_normalized(tensorFrameExample,'Fvalue')->bb;
#' tf_edgeNames(bb)->edgeNames;
#' times_data <- tf_timesVec(edgeNames);
#' times_data;
tf_timesVec <- function(data_frame,value_var){
  edges <- tf_edges(data_frame,value_var);
  #获取辅助信息,阶的序号
  edge_index <- seq_along(edges);
  #阶的总长度
  edge_len <- length(edges);
  #处理times信息
  times_data <-c(1,edges[-edge_len]);
  names(times_data) <- names(edges);
  times_counter <-lapply(edge_index, function(i){
    prod(times_data[1:i])
  })
  res <-unlist(times_counter);
  names(res) <-names(edges);
  return(res)
  
}
#' 根据edgeNames信息自动生产扩展表
#'
#' @param data_frame 数据框
#' @param value_var value字段类型
#'
#' @return 返回data_frame进行
#' @export
#'
#' @examples tf_edgeExpTable();
tf_edgeExpTable <- function(data_frame,value_var){
  edges <- tf_edges(data_frame,value_var);
  #获取辅助信息,阶的序号
  edge_index <- seq_along(edges);
  #阶的总长度
  edge_len <- length(edges);
  #处理each信息,用于控制上下行重复
  each_counter <- tf_eachVec(data_frame,value_var);
  #each_counter;
  #处理times处理
  times_counter <-tf_timesVec(data_frame,value_var);
  #times_counter;
  
  #处理最终数据
  res<- lapply(edge_index,function(i){
    element <- edgeNames[[i]];
    tpl <-rep(element,each=each_counter[i]);
    data <- rep(tpl,times_counter[i]);
  })
  res <- as.data.frame(res,stringsAsFactors=FALSE);
  names(res) <- names(edges);
  return(res)
}

#' 针对原始数据框进行插补，不排序，只显示差异部分
#'
#' @param fillValue 插入值,默认为0
#' @param data_frame 数据框
#' @param value_var  value列
#'
#' @return 返回值
#' @export
#'
#' @examples 
tf_fillData <- function(data_frame,value_var,fillValue = 0) {

  res <- tf_edgeExpTable(data_frame,value_var);
  tensorFrameFilled_flag <- df_mergeColumn(res);
  fieldCategory <- tf_fieldNames(data_frame,value_var)$category;
  data_categories <-data_frame[,fieldCategory,drop=FALSE]
  data_categories_flag <- df_mergeColumn(data_categories);
  diff <-res[!tensorFrameFilled_flag %in% data_categories_flag,];
  diff$Fvalue <-fillValue;
  #data_all<-tf_normalized$all;
  names(diff) <-names(data_frame);
  return(diff)
}
#    2.4针对数据进行合并----
#' 将原始数据与插补数据进行合并
#'
#' @param data_frame 数据框
#' @param filledData 已经插补的数据
#'
#' @return 返回值
#' @export
#'
#' @examples tf_combineData();
tf_combineData <-function(data_frame,filledData){
  res <-rbind(data_frame,filledData)
  return(res)
}
#    2.5针对数据进行排序----

#    2.6生成data数据-------
