
# maybe into "functions-vector.R"

vector.getLast = function (vector, n.out = 1) 
	{
    n = length(vector);
    x[sign(n.out) * (n - abs(n.out) + 1):n]
	}


## vector.push_back(element) is C++
push_back = function(element, vector)
	{
	c(vector, element);
	}

#' @rdname push_last
#' @export
push_last = push_back;

push_front = function(element, vector)
	{
	c(element, vector);
	}

#' @rdname push.first
#' @export
push_first = push_front;






