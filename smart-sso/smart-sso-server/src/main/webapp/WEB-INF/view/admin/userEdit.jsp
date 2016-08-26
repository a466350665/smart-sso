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
		${empty user.id ? '添加' : '修改'}管理员
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<!-- PAGE CONTENT BEGINS -->
		<form id="_editForm" class="form-horizontal" role="form" 
			validate="true">
			<input type="hidden" name="id" value="${user.id}">
			
			<div class="form-group">
				<label for="_account" class="col-sm-3 control-label no-padding-right">登录名</label>

				<div class="col-sm-9">
					<div class="input-medium help-validate">
						<div class="input-group">
							<input id="_account" name="account" type="text" value="${user.account}" class="form-data"
								ajax="{url : '${_path}/admin/user/validateAccount', dataId : '_editForm'}"
								required="true" minlength = '4' maxlength = '64'/>
							<span class="input-group-addon">
								<i class="ace-icon fa fa-user"></i>
							</span>
						</div>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label for="_password" class="col-sm-3 control-label no-padding-right">密码</label>
				
				<div class="col-sm-9">
					<div class="input-medium help-validate">
						<div class="input-group">
							<input id="_password" name="password" type="password" value="" class="form-data"
								${!empty user.id ? 'data-rel="tooltip" title="不修改请留空"' : ''}
								${empty user.id ? 'required="true"' : ''} minlength = '6' maxlength = '16'/>
							<span class="input-group-addon">
								<i class="ace-icon fa fa-lock"></i>
							</span>
						</div>
					</div>
				</div>
			</div>
			
			<div class="form-group">
				<label class="control-label col-xs-12 col-sm-3 no-padding-right">是否启用</label>

				<div class="col-xs-12 col-sm-9">
					<div class="clearfix help-validate">
						<div>
							<label class="line-height-1 blue">
								<input name="isEnable" value="true" type="radio" class="ace" ${user.isEnable ? 'checked="checked"' : ''}/>
								<span class="lbl"> 是</span>
							</label>
						</div>
	
						<div>
							<label class="line-height-1 blue">
								<input name="isEnable" value="false" type="radio" class="ace" ${!user.isEnable ? 'checked="checked"' : ''}/>
								<span class="lbl"> 否</span>
							</label>
						</div>
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
			//焦点
			$("#_account").focus();
			
			//帮助查看
			$('[data-rel=tooltip]').tooltip({container:'body'});
			
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/user/save", $.formJson('_editForm'),function(d) {
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
			
			// 回车绑定
			$(".form-data").bind('keypress',function(event){
                if(event.keyCode == "13"){
                	event.preventDefault();
                	$("#_submit").click();
                }
            });
		});
	});
</script>
