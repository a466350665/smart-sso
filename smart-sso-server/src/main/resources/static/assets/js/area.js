(function($){
	var initOpt = "<option value=\"\">请选择</option>";
	//插件初始化
	$.fn.areaInit = function(obj) {
		obj = $.extend({}, obj, {synchro : $(this)});
		if(!obj) {alert("下拉列表参数不能为空！");return;}  
		if(!obj.selectIds) {alert("数组元素selectIds不能为空！");return;} 
		if(!obj.synchro){alert("数组元素synchro不能为空！");return;}
		var synchroDB = $('#'+obj.dbAreaCode);
		var synchro = obj.synchro;
		var selectIds = obj.selectIds;
		
		var province;
		var city;
		var district;
		
		var fUrl = window.location.href;
		var filterFront = fUrl.indexOf("front");
		if ( filterFront> 0 ){
			fUrl = fUrl.substring(0,filterFront);
		}     
		var filterAdmin = fUrl.indexOf("admin");
		if ( filterAdmin> 0 ){
			fUrl = fUrl.substring(0,filterAdmin);
		}
		
		//向后台请求所有城市的列表
		$.ajax({
			type: 'POST', 
			url: fUrl+"front/area!initDownList.shtml",
			async:false,
			dataType: 'json', 
			success: function(data, textStatus) {
				province = data.province;
				city = data.city;
				district = data.district;
			}
		});
		
		//第一个下拉列表
		var firstSelectId = selectIds[0];
		//第二个下拉列表
		var secondSelectId = selectIds.length>1?selectIds[1]:[];
		//第三个下拉列表
		var threeSelectId = selectIds.length>2?selectIds[2]:[];
		
		obj.city = city;
		obj.district = district;
		
		var firstOption = initOpt;
		var secondOption = initOpt;
		var threeOption = initOpt;
			
			//加载一级下拉列表
		jQuery.each(province, function(k, v)  
		{  
			firstOption += "<option value=\"" + v.code + "\">" + v.name + "</option>";  
		}); 
		
		$("#"+firstSelectId).append(firstOption);  
	    $("#"+firstSelectId).change(function()  
	    {  
	    	//根据一级列表加载二级下拉列表
	        show_secondLevelInfo(obj,$(this).val(),secondSelectId,threeSelectId);  
	    }); 
	    
	    var synchroIdVal = synchro.val();
	    if(!synchroIdVal){  
	    	/* 新建模式 */
			show_secondLevelInfo(obj,$("#"+firstSelectId).val(),secondSelectId,threeSelectId);
		    show_threeLevelInfo(obj,$("#"+secondSelectId).val(),threeSelectId);
		} else {  
			//编辑模式
      		var synchroIdArray = synchroIdVal.split(",");
      		var firstsynchroId = synchroIdArray[0];
      		var secondsynchroId = synchroIdArray.length>1?synchroIdArray[1]:[];
      		var threesynchroId = synchroIdArray.length>2?synchroIdArray[2]:[];
      		
      		$("#"+firstSelectId).val(firstsynchroId);
      		show_secondLevelInfo(obj,$("#"+firstSelectId).val(),secondSelectId,threeSelectId);
      		$("#"+secondSelectId).val(secondsynchroId);
      		show_threeLevelInfo(obj,$("#"+secondSelectId).val(),threeSelectId);
      		$("#"+threeSelectId).val(threesynchroId);
      	}
	    
	    //二级下拉列表
	    function show_secondLevelInfo(obj,value,selectId,nextSelectId){
	    	secondOption = initOpt;
	    	jQuery.each(obj.city, function(k, v){
	    		if(v.parentCode == value){
	    			secondOption += "<option value=\"" + v.code + "\">" + v.name + "</option>";
	    		}
	    	});
	    	$("#"+selectId).empty();
	    	$("#"+selectId).append(secondOption); 
	    	
	    	$("#"+selectId).change(function()  
			{  
	    		if(nextSelectId.length == 0){
	    			//设置实际地址编码
		    		obj.synchro.val($(this).val());
		    		synchroDB.val($(this).val());
	    		}else{
	    			//根据二级列表加载三级下拉列表
	    			show_threeLevelInfo(obj,$(this).val(),nextSelectId);  
	    		}
			}); 
	    	
	    	show_threeLevelInfo(obj,$("#"+selectId).val(),nextSelectId);
	    }
	    
	    //加载三级下拉列表
	    function show_threeLevelInfo(obj,value,selectId){
	    	threeOption = initOpt;
	    	jQuery.each(obj.district, function(k, v){
	    		if(v.parentCode == value){
	    			threeOption += "<option value=\"" + v.code + "\">" + v.name + "</option>";
	    		}
	    	});
	    	$("#"+selectId).empty();
	    	$("#"+selectId).append(threeOption);  
	    	
	    	$("#"+selectId).change(function()  
			{  
	    		//设置实际地址编码
	    		obj.synchro.val($(this).val());
	    		synchroDB.val($(this).val());
			}); 
	    }
	}
})(jQuery);

