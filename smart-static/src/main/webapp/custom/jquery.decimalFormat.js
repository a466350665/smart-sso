/**
 * 小数点后处理，少位补零
 */
function add0(value, len, str){
	var len0 = len; 
	if(value && value.length <= len){
		len0 -= value.length;
	}
	for(var i = 0;i < len0; i++){
		value += str;
	}
	return value;
}

/**
 * 小数点前处理，三位添加分隔
 */
function addSeparator(v, separator){
	var minus = "";
	if(v.substr(0,1) == "-"){
		v = v.substring(1, v.length);
		minus = "-";
	}
	if(v.length < 3)
		return minus + v;
	var temp = "";
	for(var i = v.length; i > 0; i = i - 3){
		if(i>3){
			temp = (separator + v.substring(i-3, i)) + temp;
		}
		else{
			temp = v.substring(0, i) + temp;
		}
	}
	return minus + temp;
}
/**
 * 小数格式化
 */
function decimalFormat(value, place, separator){
	separator = separator ? separator : "";
	place = place ? place : 2;
	var t =  Math.round(value*Math.pow(10,place))/Math.pow(10,place);
	var ts = t.toString().split(".");
	return addSeparator(ts[0], separator) + "." + add0(ts[1] ? ts[1] : "", place, "0");
}

function _formatMoney(value){
	return decimalFormat(value, 2, ",");
}

(function($) {
	$.decimalFormat = function() {
		// 对所有金额格式化
		$(".amount").each(function(j,d) {
			$(d).html(_formatMoney($(d).html()));
		});
	};
})(jQuery);