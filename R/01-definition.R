# 定义一个张量包tensorBag作为基本的存储单元----
#' 定义一个张量包，这是张量在R中的基本单元
#'
#' @slot edgeCount integer. 张量的阶数
#' @slot edges integer.  张量的每一阶的维数，可以不一样
#' @slot edgeNames character. 张量的初始名称，系统自动命名
#' @slot data array. 张量的数据存储，在tensor.toolkit中使用array的type.
#' @slot dimNames 维度名称
#' @slot custNames 用户定义的维度名称
#' @slot nickNames 自动定义的维度名称
#' @slot useNick 是否使用自动定义的维度名称
#' @slot full_capacity 张量理论容量
#' @slot real_capacity 张量实际容量
#' @slot diff_capacity 张量差异容量
#' @slot useFill 是否需要填充
#' @slot fillData 填充数据
#' @slot edgeRank 阶排序
#' @slot dimRank  维排序
#' @slot fillValue 插补值
#' @slot valueName 值列名称 值名称
#' @slot dimExpTable 维度扩展表
#' @slot srcClass 源数据类型
#' @slot synChkTable 同步检查表
#'
#' @return 返回值
#' @export
#'
#' @examples tensorBag();
tensorBag <-setClass("tensorBag", 
                     slots = c(edgeCount = "integer",
                               edges="integer",
                               edgeNames='character',
                               edgeRank='integer',
                               dimNames='list',
                               custNames='list',
                               nickNames='list',
                               useNick='logical',
                               dimRank='list',
                               full_capacity='integer',
                               real_capacity='integer',
                               diff_capacity='integer',
                               useFill='logical',
                               fillValue='numeric',
                               fillData='data.frame',
                               valueName='character',
                               srcClass='character',
                               dimExpTable='data.frame',
                               synChkTable='data.frame',
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
