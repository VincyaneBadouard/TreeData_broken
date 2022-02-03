#' GenerateComment
#'
#' @description For all rows concerned by the condition, the function add a
#'   string in the "Comment" column of a data.table. If there is already a
#'   value, the string is pasted after the "/" separator.
#'
#' @param Data (data.table)
#' @param condition (logical)
#' @param comment The string to add in the "Comment" column (character)
#'
#' @return The input data.table with the filled "Comment" column.
#'
#' @export
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(A = c(2, 1, 3), B = c(6, 10, 6))
#'
#' dt[A == 2,
#'    "Comment" := paste0("A = 2")] # 1st comment
#'
#' GenerateComment(dt, condition = dt$B == 6, comment = "B = 6")
#'}
GenerateComment <- function(Data, condition, comment){

  if(!"Comment" %in% names(Data)) Data$Comment <- ""

  # Apply the function 'CommentByRow' by row
  for (r in 1:nrow(Data[condition,])) {
    Data[condition,][r,] <- CommentByRow(Data[condition,][r,], comment)
  }
  return(Data)
}


#' CommentByRow
#'
#' @description Add a string in the "Comment" column of a data.table.
#' If there is already a value, the string is pasted after the "/" separator.
#'
#' @param row A 1 row data.table (data.table)
#' @param comment The string to add in the "Comment" column (character)
#'
#' @return The input data.table with the filled "Comment" column.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(A = c(2), B = c(6))
#'
#' dt <- CommentByRow(dt, comment = "A = 2") # 1st comment
#'
#' CommentByRow(dt, comment = "B = 6")
#'}
CommentByRow <- function(row, comment){

  if(!"Comment" %in% names(row)) row$Comment <- ""

  row[, "Comment"] <- ifelse(row[, "Comment"] == "",
                             comment,
                             paste(row[, "Comment"], comment, sep ="/"))
  return(row)
}

# A <- paste0("A")
# B <- c(A, paste0("B"))
#
# # 1st example
# dt <- data.table(A = c(2, 1), B = c(6, 10))
#
# dt[A == 2,
#    "Comment" := paste0("A = 2")] #  1st comment
# dt[B == 6,
#    "Comment" := paste0("B = 6")] # new comment
#
# dt[B == 6,
#    ("Comment") := c(paste0("B = 6"))] # comments
#
# dt[B == 6]$Comment <- paste0(dt[B == 6]$Comment, ",", "B = 6")
#
# # 2nd example
# dt <- data.table(A = c(2, 1, 3), B = c(6, 10, 6))
#
# dt[A == 2,
#    "Comment" := paste0("A = 2")] # 1st comment
#
# dt[B == 6,
#    "Comment" := paste0(dt[B == 6]$Comment, ",", "B = 6")] # add a comment without errase the first
