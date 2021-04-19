
#' message.stop
#'
#' This combines 'stop' and 'paste0'
#'
#' @param ... string elements
#' @param collapse
#' @param recycle0
#' @param pre
#'
#' @return
#' @export
message.stop = function(... , collapse = NULL, recycle0 = FALSE, pre=TRUE)
  {
  str.pre = str.post = "";
  if(pre)
	{
	str.pre  = paste0("\n", "====================== ERROR ======================", "\n");
	str.post = paste0("\n", "===================================================", "\n");
	}
  stop( paste0(str.pre, ..., str.post, collapse = collapse, recycle0 = recycle0), call. = FALSE );
  }

#' message.warning
#'
#' This combines 'warning' and 'paste0'
#'
#' @param ... string elements
#' @param collapse
#' @param recycle0
#' @param pre
#'
#' @return
#' @export
message.warning = function(... , collapse = NULL, recycle0 = FALSE, pre=TRUE)
  {
  str.pre = str.post = "";
  if(pre)
	{
	str.pre  = paste0("\n", "====================== WARNING ======================", "\n");
	str.post = paste0("\n", "=====================================================", "\n");
	}
  warning( paste0(str.pre, ..., str.post, collapse = collapse, recycle0 = recycle0), call. = FALSE );
  }


