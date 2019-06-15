#' 将vector数据转化为tensorBag
#'
#' @param data 向量数据
#' @param edgeNames 维度名称
#'
#' @return 返回值
#' @include 01-tensorBagDefinition.R
#' @include 01-constructor.R
#'
#' @examples tensorBag_Vector();
tensorBag_vector <- function(data,edgeNames){
  
  res1 <-new('tensorBag');
  edgeCount <- 1L;
  edges <- as.integer(length(data));
  edgeNames<-edgeNames;
  data <- array(data,dim = edges,dimnames = edgeNames);
    res2 <- initialize(res1,edgeCount = edgeCount,
                     edges=edges,
                     edgeNames=edgeNames,
                     data=data)
  return(res2)
  
}

#' 将matrix转换成tensorBag
#'
#' @param data matrix数据
#' @param edgeNames 维度名称
#'
#' @return 返回值
#' @export
#'
#' @examples tensorBag();
tensorBag_matrix <- function(data,edgeNames){
  res1 <-new('tensorBag');
  edgeCount <- 2L;
  edges <- as.integer(dim(data));
  edgeNames<-edgeNames
  data <- array(data,dim = edges,dimnames = edgeNames);
  res2 <- initialize(res1,edgeCount = edgeCount,
                     edges=edges,
                     edgeNames=edgeNames,
                     data=data)
  return(res2)
  
  
}

#' 将array转换tensorBag
#'
#' @param data  array数据
#'
#' @return 返回值
#' @export
#'
#' @examples tensorBag();
tensorBag_array <- function(data,edgeNames){
  res1 <-new('tensorBag');
  edges <- as.integer(dim(data));
  edgeCount <- length(edges);
  edgeNames<-edgeNames;
  data <- array(data,dim = edges,dimnames = edgeNames);
  res2 <- initialize(res1,edgeCount = edgeCount,
                     edges=edges,
                     edgeNames=edgeNames,
                     data=data)
  return(res2)
  
  
}

