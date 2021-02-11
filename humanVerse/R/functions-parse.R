# slice and dice


# function sliceDice($str, $start="<h2>",$end="</h2>", $strip=TRUE, $direction="end")
# 		{		# what if <h2 without >
# 		if($direction == "end")
# 			{
# 			$tmp = explode($end,$str);
# 				$tmp1 = explode($start,$tmp[0]);
# 				if(!isset($tmp1[1])) { $tmp1[1] = ""; }
# 			$str = $tmp1[1];
# 			} else { # start ....
# 					$tmp = explode($start,$str);
# 					if(!isset($tmp[1])) { $tmp[1] = ""; }
# 						$tmp1 = explode($end,$tmp[1]);
# 					$str = $tmp1[0];
# 					}
#
# 			if($strip) { $str = trim(strip_tags($str)); }
# 		return $str;
# 		}
