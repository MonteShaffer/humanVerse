


msg.badLength = function() {} # this will stop 
msg.unequalLength = function() {} # this will warn ...


msg.badOption = function(KEY="method", 
								OPTION="Original-Entry", 
								SHORTCODE="ori-ent")
		{
		prep.msg("\n\n\t",
					"It appears that you entered an INCORRECT",
					"<v>[</v>", KEY, "<v>]</v>", 
					"\n\n\t",
					"You entered: ", "<v>[</v>", OPTION, "<v>]</v>",
					"\n\n\t\t",
					"which was 'short-coded' to: ", "<v>[</v>", SHORTCODE, "<v>].</v>",
					"\n\n",
					"Please try again.",
					"\n\n",					
					"Above is a list of options with allowed 'shortcodes'.",
					"\n\n",
					"Which can be retrieved as a dataframe using ",
					"<v>* ANS *</v> ", 
					"\n\n"
					);		
		}
	
	

msg.missingParam = function(PARAM="f", DEFAULT=1)
		{
		prep.msg("\n\n\t",
					"It appears that you are missing a PARAMETER",
					"<v>[</v>", PARAM, "<v>]</v>", 
					"\n\n\t",
					"Assigning to DEFAULT value ", "<v>[</v>", DEFAULT, "<v>]</v>",
					"\n\n\t\t",
					"The result may not be what you intended.  You may want to try again.",
					"\n\n"
					);		
		}


























































































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


