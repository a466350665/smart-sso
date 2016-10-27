<%@ page language="java" pageEncoding="utf-8"%>
<title>应用-${_systemName}</title>

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
		${empty app.id ? '添加' : '修改'}应用
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<!-- PAGE CONTENT BEGINS -->
		<form id="_editForm" class="form-horizontal" role="form" 
			validate="true">
			<input type="hidden" name="id" value="${app.id}"> 
			
			<div class="form-group">
				<label for="_name" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>名称</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_name" name="name" type="text" value="${app.name}" class="form-data col-xs-10 col-sm-5" placeholder="应用名称"
							required="true" maxlength="64"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label for="_code" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>编码</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_code" name="code" type="text" value="${app.code}" class="form-data col-xs-10 col-sm-5" placeholder="应用编码"
							required="true" ajax="{url : '${_path}/admin/app/validateCode', type : 'post', dataId : '_editForm'}" maxlength="64"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label for="_sort" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>排序</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_sort" name="sort" type="text" value="${app.sort}" class="form-data col-xs-10 col-sm-5" placeholder="排序"
							required="true" vtype="integer" min="1" max="9999"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label class="control-label col-xs-12 col-sm-3 no-padding-right"><span class="form-star">*</span>是否启用</label>

				<div class="col-xs-12 col-sm-9">
					<div class="clearfix help-validate">
						<div>
							<label class="line-height-1 blue">
								<input name="isEnable" value="true" type="radio" class="ace" ${app.isEnable ? 'checked="checked"' : ''}/>
								<span class="lbl"> 是</span>
							</label>
						</div>
	
						<div>
							<label class="line-height-1 blue">
								<input name="isEnable" value="false" type="radio" class="ace" ${!app.isEnable ? 'checked="checked"' : ''}/>
								<span class="lbl"> 否</span>
							</label>
						</div>
					</div>
				</div>
			</div>
			
			<div class="clearfix form-actions">
				<div class="col-md-offset-3 col-md-9">
					<button id="_submit" type="button" class="btn btn-info" data-loading-text="正在提交..." permission="/app/save">
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
			$("#_name").focus();
			
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/app/save", $.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							if(d.status == '0000'){
								window.location.href = "${_path}/admin/admin#/admin/app";
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
				window.location.href = "${_path}/admin/admin#/admin/app";
			});
			
			// 回车绑定
			$(".form-data").bind('keypress',function(event){
                if(event.keyCode == "13"){
                	event.preventDefault();
                	$("#_submit").click();
                }
            });
            
            // 权限处理
			$.permission();
		});
	});
</script>
