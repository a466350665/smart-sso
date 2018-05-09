/**
 * jQuery multiselect custom
 * 
 * Author Joe
 */
jQuery(function($) {
	var def = {
		// 显示搜索框
		enableFiltering: false,
		nonSelectedText: '请选择',
		nSelectedText: '项已选择',
		allSelectedText: '全选',
		// 显示几项
		numberDisplayed: 10,
		// 显示全选
		includeSelectAllOption: false,
		// 显示全选 文字
		selectAllText: '选择全部',
		onChange: function(option, checked, select) {
			// 改变的时候验证
			if(smart && smart.validate){
				$(option).parent().validate();
			}
		},
		buttonClass: 'btn btn-white',
		templates: {
			button: '<button type="button" class="multiselect dropdown-toggle" data-toggle="dropdown"></button>',
			ul: '<ul class="multiselect-container dropdown-menu"></ul>',
			filter: '<li class="multiselect-item filter"><div class="input-group"><span class="input-group-addon"><i class="fa fa-search"></i></span><input class="form-control multiselect-search" type="text"></div></li>',
			filterClearBtn: '<span class="input-group-btn"><button class="btn btn-default btn-white btn-grey multiselect-clear-filter" type="button"><i class="fa fa-times-circle red2"></i></button></span>',
			li: '<li><a href="javascript:void(0);"><label></label></a></li>',
			divider: '<li class="multiselect-item divider"></li>',
			liGroup: '<li class="multiselect-item group"><label class="multiselect-group"></label></li>'
		 }
	};
	
	$.select = function($o, c) {
		// 多选框
		$o.multiselect($.extend({}, def, c));
	};
	
	$.fn.select = function(c) {
		return $.select($(this), c);
	};
});
