<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="分配角色"/>
</jsp:include>

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
			<input type="hidden" name="userId" value="${user.id}">
			
			<div class="form-group">
				<label for="_name" class="col-sm-3 control-label no-padding-right">姓名</label>

				<div class="col-sm-6">
					<div class="clearfix help-validate">
						<input id="_name" name="name" type="text" value="${user.name}" class="form-data col-xs-10 col-sm-5" placeholder="名称"
							maxlength="50" disabled="disabled"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label class="col-sm-3 control-label no-padding-right">角色</label>
				
				<div class="col-xs-12 col-sm-9">
					<div class="clearfix help-validate">
						<c:forEach var="item" items="${roleList}">
							<label>
								<input name="roleId" value="${item.id}" type="checkbox" class="ace" ${item.checked ? 'checked="checked"' : ''}/>
								<span class="lbl">&nbsp;&nbsp;${item.name}</span>
							</label>
						</c:forEach>
						<input id="_roleIds" type="hidden" name="roleIds" value="">
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
					$.post("${_path}/admin/userRole/save", $.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							if(d.code == 1){
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
