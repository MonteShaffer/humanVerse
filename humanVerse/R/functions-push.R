

push.back = function(vector, element)
	{
	c(vector, element);
	}

#' @rdname push.last
#' @export
push.last = push.back;

push.front = function(vector, element)
	{
	c(element, vector);
	}

#' @rdname push.first
#' @export
push.first = push.front;






