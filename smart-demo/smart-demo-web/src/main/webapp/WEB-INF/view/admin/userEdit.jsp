<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="用户"/>
</jsp:include>

<div class="page-header">
	<h1>
		${empty user.id ? '添加' : '修改'}用户
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<form id="_editForm" class="form-horizontal" role="form" 
			validate="true">
			<input type="hidden" name="id" value="${user.id}">
			
			<div class="form-group">
				<label for="_account" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>登录名</label>

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

<script type="text/javascript">
	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
			//焦点
			$("#_account").focus();
			
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/user/save", $.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							if(d.code == '0000'){
								$.aceRedirect("${_path}/admin/user");
							}
							else {
								$.gritter.add({text: d.message});
							}
						}
			        },'json');
				}
			});
			
			// 取消
			$("#_cancle").click(function(){
				$.aceRedirect("${_path}/admin/user");
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
