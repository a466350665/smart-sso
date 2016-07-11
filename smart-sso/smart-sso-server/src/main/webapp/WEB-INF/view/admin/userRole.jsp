<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<title>管理员-${_systemName}</title>

<!-- 文本框 -->
<link rel="stylesheet" href="${_staticPath}/assets/css/jquery-ui.custom.css" />
<!-- 多选框 -->
<link rel="stylesheet" href="${_staticPath}/assets/css/bootstrap-multiselect.css" />
<!-- 颜色选择  -->
<link rel="stylesheet" href="${_staticPath}/assets/css/colorpicker.css" />
<!-- 时间  -->
<link rel="stylesheet" href="${_staticPath}/assets/css/bootstrap-timepicker.css" />
<!-- 日期、日期+时间、日期范围  -->
<link rel="stylesheet" href="${_staticPath}/assets/css/daterangepicker.css" />


<div class="page-header">
	<h1>
		分配角色
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<!-- PAGE CONTENT BEGINS -->
		<form id="_editForm" class="form-horizontal" role="form" 
			validate="true">
			<input id="_userId" type="hidden" name="userId" value="${userId}">
			<input id="_roleIds" type="hidden" name="roleIds">
			
			<div class="form-group">
				<label for="_appId" class="col-sm-3 control-label no-padding-right">应用</label>

				<div class="col-sm-3">
					<select id="_appId" name="appId" class="form-control help-validate"
						required="true">
						<c:if test="${empty appList}">
							<option value="">--请选择--</option>
						</c:if>
						<c:forEach var="item" items="${appList}">
							<option value="${item.id}">${item.name}</option>
						</c:forEach>
					</select>
				</div>
			</div>
			
			<div class="form-group">
				<div class="col-xs-12 col-sm-9">
					<div id="_roleDiv" class="clearfix help-validate">
						<c:forEach var="item" items="${roleList}">
							<div class='col-sm-2'>
								<label>
									<input name="roleId" value="${item.id}" type="checkbox" class="ace" ${item.isChecked ? 'checked="checked"' : ''}/>
									<span class="lbl">&nbsp;&nbsp;${item.name}</span>
								</label>
							</div>
						</c:forEach>
					</div>
				</div>
			</div>
			
			<div class="clearfix form-actions">
				<div class="col-md-offset-3 col-md-9">
					<button id="_submit" type="button" class="btn btn-info" data-loading-text="正在提交...">
						<i class="ace-icon fa fa-check bigger-110"></i>
						提交
					</button>

					&nbsp; &nbsp; &nbsp;
					<button id="_cancle" class="btn" type="reset">
						<i class="ace-icon fa  fa-times bigger-110"></i>
						取消
					</button>
				</div>
			</div>
		</form>

	</div>
</div>


<!--[if lte IE 8]>
  <script src="${_staticPath}/assets/js/excanvas.js"></script>
<![endif]-->
<script type="text/javascript">
	var scripts = [null,
		// UI
		"${_staticPath}/assets/js/jquery-ui.custom.js?v=" + Math.random(),
		// Form提交Json转换
		"${_staticPath}/script/jquery.form.min.js?v=" + Math.random(),
		// 验证
		"${_staticPath}/script/jquery.validate-2.0.min.js?v=" + Math.random(),
		// 验证定制
		"${_staticPath}/script/jquery.validate-2.0.custom.min.js?v=" + Math.random(),
		null];
	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
			$("#_appId").change(function () { 
				$.ajax({
            		url:"${_path}/admin/userRole/change",
            		type:"get",
            		data:$.formJson('_editForm'),
            		dataType:"json",
            		success:function(d){
            			if(d){
            				var data = d.data;
            				$("#_roleDiv").html('');
            				var html= '';
            				for(var i=0; i<data.length; i++){
	            				html += '<div class="col-sm-2">';
	            				html += '	<label>';
	            				html += '		<input name="roleId" value="' + data[i].id + '" type="checkbox" class="ace" ';
	            				if(data[i].isChecked){
	            					html += 'checked="checked"';
	            				}
	            				html += '		/>';
	            				html += '		<span class="lbl">&nbsp;&nbsp;' + data[i].name + '</span>';
	            				html += '	</label>';
	            				html += '</div>';
            				}
							$("#_roleDiv").append(html);
            			}
            		}
            	});
           	});
			
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var roleIds = "";
					$("input[name='roleId']:checked").each(function(i, d){
						if(i > 0){
							roleIds += ",";
						}
						roleIds += $(this).val();
					});
					$("#_roleIds").val(roleIds);
					
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/userRole/allocateSave", $.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							if(d.status == '0000'){
								window.location.href = "${_path}/admin/admin#/admin/user";
							}
							else {
								location.reload();
							}
						}
			        },'json');
				}
			});
			
			// 取消
			$("#_cancle").click(function(){
				window.location.href = "${_path}/admin/admin#/admin/user";
			});
		});
	});
</script>
