# 定义一个张量包tensorBag作为基本的存储单元----
#' 定义一个张量包，这是张量在R中的基本单元
#'
#' @slot edgeCount integer. 张量的阶数
#' @slot edges integer.  张量的每一阶的维数，可以不一样
#' @slot edgeNames character. 张量的初始名称，系统自动命名
#' @slot data array. 张量的数据存储，在tensor.toolkit中使用array的type.
#'
#' @return 返回值
#' @export
#'
#' @examples tensorBag();
tensorBag <-setClass("tensorBag", 
                     slots = c(edgeCount = "integer",
                               edges="integer",
                               edgeNames='list',
                               data="array"
))

#' 定义张量列表
#'
#' @slot data list.张量列表数据，每个子类类型应该为tensorBag或tensorList 
#' @slot itemType character. 元素类型，一般为tensorBag或tensorList
#' @slot itemCount integer. 元素个数
#' @slot listLevel 列表级次，默认为1
#' @slot listNames 列表名称
#' @slot is_detail 是否明细当listLevel为1,itemType为tensorBag为TRUE
#'
#' @return 返回值
#' @export
#'
#' @examples tensorList();
tensorList <- setClass('tensorList',slots = c(
                                              listNames='character',
                                              listLevel='integer',
                                              itemType='character',
                                              itemCount='integer',
                                              is_detail='logical',
                                              data='list'
                                              ))
