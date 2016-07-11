<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%  
    response.setHeader("Pragma", "No-cache");
    response.setHeader("Cache-Control", "no-cache");
    response.setDateHeader("Expires", 0);   
%>
<jsp:include page="../assets/common/head.jsp">
	<jsp:param name="title" value="_&{empty ${_model!''}.id ? '添加' : '修改'}${tableComment!''}"/>
</jsp:include>

<div class="page-header">
	<h1>
		_&{empty ${_model!''}.id ? '添加' : '修改'}${tableComment!''}
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<form id="_editForm" class="form-horizontal" role="form" method="post" 
			validate="true">
			<input type="hidden" name="${_model!''}.id" value="_&{${_model!''}.id}"> 
			
			<#list fieldList as field>
		        <#if field.fieldType == "Boolean">
			<div class="form-group">
				<label class="control-label col-xs-12 col-sm-3 no-padding-right">${field.description!''}</label>

				<div class="col-xs-12 col-sm-9">
					<div class="clearfix help-validate">
						<div>
							<label class="line-height-1 blue">
								<input name="${_model!''}.${field.fieldName!''}" value="true" type="radio" class="ace" _&{${_model!''}.${field.fieldName!''} ? 'checked="checked"' : ''}/>
								<span class="lbl"> 是</span>
							</label>
						</div>
	
						<div>
							<label class="line-height-1 blue">
								<input name="${_model!''}.${field.fieldName!''}" value="false" type="radio" class="ace" _&{!${_model!''}.${field.fieldName!''} ? 'checked="checked"' : ''}/>
								<span class="lbl"> 否</span>
							</label>
						</div>
					</div>
				</div>
			</div>
				<#elseif field.fieldType == "Date">
			<div class="form-group">
				<label class="col-sm-3 control-label no-padding-right" for="_${field.fieldName!''}">${field.description!''}</label>

				<div class="col-sm-9">
					<div class="input-medium help-validate">
						<div class="input-group">
							<input id="_${field.fieldName!''}" class="input-medium" type="text"
								<#if field.nullableStr == "false">required="true"</#if>/>
							<span class="input-group-addon">
								<i class="ace-icon fa fa-calendar"></i>
							</span>
						</div>
					</div>
				</div>
			</div>
				<#elseif field.fieldType == "Integer">
			<div class="form-group">
				<label for="_${field.fieldName!''}" class="col-sm-3 control-label no-padding-right">${field.description!''}</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_${field.fieldName!''}" name="${_model!''}.${field.fieldName!''}" type="text" value="_&{${_model!''}.${field.fieldName!''}}" class="form-data col-xs-10 col-sm-5" placeholder="${field.description!''}"
							<#if field.nullableStr == "false">required="true"</#if> vtype="integer"<#if field.intMaxLengthStr??> maxlength="${field.intMaxLengthStr}"</#if>/>
					</div>
				</div>
				
			</div>
				<#elseif field.fieldType == "Double">
			<div class="form-group">
				<label for="_${field.fieldName!''}" class="col-sm-3 control-label no-padding-right">${field.description!''}</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_${field.fieldName!''}" name="${_model!''}.${field.fieldName!''}" type="text" value="_&{${_model!''}.${field.fieldName!''}}" class="form-data col-xs-10 col-sm-5" placeholder="${field.description!''}"
							<#if field.nullableStr == "false">required="true"</#if> vtype="double"<#if field.maxLengthStr??> maxlength="${field.maxLengthStr}"</#if>/>
					</div>
				</div>
				
			</div>
				<#else>
			<div class="form-group">
				<label for="_${field.fieldName!''}" class="col-sm-3 control-label no-padding-right">${field.description!''}</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_${field.fieldName!''}" name="${_model!''}.${field.fieldName!''}" type="text" value="_&{${_model!''}.${field.fieldName!''}}" class="form-data col-xs-10 col-sm-5" placeholder="${field.description!''}"
							<#if field.nullableStr == "false">required="true"</#if> <#if field.maxLengthStr??> maxlength="${field.maxLengthStr}"</#if>/>
					</div>
				</div>
				
			</div>
				</#if>
			</#list>
			
			<div class="clearfix form-actions">
				<div class="col-md-offset-3 col-md-9">
					<button id="_submit" type="button" class="btn btn-info" data-loading-text="正在提交..." permission="${_model!''}!save.shtml">
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
  <script src="${path}/assets/js/excanvas.js"></script>
<![endif]-->
<script type="text/javascript">
	var scripts = [null,
		// UI
		"${path}/assets/js/jquery-ui.custom.js?v=" + Math.random(),
		// Form提交Json转换
		"${path}/assets/js/jquery.form.min.js?v=" + Math.random(),
		// 时间
		"${path}/assets/js/date-time/bootstrap-timepicker.js",
		// 时间支持
		"${path}/assets/js/date-time/moment.js",
		// 时间支持
		"${path}/assets/js/date-time/i18n/moment.zh-CN.js",
		// 日期范围
		"${path}/assets/js/date-time/daterangepicker.js",
		// 验证
		"${path}/assets/js/jquery.validate-2.0.min.js?v=" + Math.random(),
		// 验证定制
		"${path}/assets/js/jquery.validate-2.0.custom.min.js?v=" + Math.random(),
		null];
	_&('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function(_&) {
			//焦点
			_&("#_name").focus();
			
			// 提交
			_&("#_submit").click(function(){
				if(_&('#_editForm').validate()){
					var btn = _&(this);
					btn.button('loading');
					_&.post("${path}/admin/${_model!''}!save.shtml", _&.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							if(d.status == 'success'){
								window.location.href = "${path}/admin/admin.shtml#page/${_model!''}.shtml";
							}
							else {
								location.reload();
							}
						}
			        },'json');
				}
			});
			
			<#list fieldList as field>
		        <#if field.fieldType == "Date">
			// 日期+时间
			_&('#_${field.fieldName!''}').daterangepicker({
				singleDatePicker: true,
				'applyClass' : 'btn btn-info',
				'cancelClass' : 'btn',
				locale: {
					applyLabel: '确定',
					cancelLabel: '取消',
					fromLabel: '自',
	                toLabel: '至'
				},
				timePicker: true,
				format: 'YYYY-MM-DD HH:mm:ss'
			}, function(start, end, label) {
                _&('#_${field.fieldName!''}').validate();
            });
            	</#if>
			</#list>
			
			// 取消
			_&("#_cancle").click(function(){
				window.location.href = "${path}/admin/admin.shtml#page/${_model!''}.shtml";
			});
			
			// 回车绑定
			_&(".form-data").bind('keypress',function(event){
                if(event.keyCode == "13"){
                	event.preventDefault();
                	_&("#_submit").click();
                }
            });
            
            // 权限处理
			_&.permission();
			// 金额格式化
			_&.decimalFormat();
		});
	});
</script>



