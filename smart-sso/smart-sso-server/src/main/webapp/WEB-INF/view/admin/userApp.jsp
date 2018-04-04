<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="用户"/>
</jsp:include>

<div class="page-header">
	<h1>
		分配应用
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<!-- PAGE CONTENT BEGINS -->
		<form id="_editForm" class="form-horizontal" role="form" 
			validate="true">
			<input type="hidden" name="userId" value="${userId}">
			<input id="_appIds" type="hidden" name="appIds" value="">
			
			<div class="form-group">
				<div class="col-xs-12 col-sm-9">
					<div class="clearfix help-validate">
						<c:forEach var="item" items="${appList}">
							<div class='col-sm-2'>
								<label>
									<input name="appId" value="${item.id}" type="checkbox" class="ace" ${item.isChecked ? 'checked="checked"' : ''}/>
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

<script type="text/javascript">
	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var appIds = "";
					$("input[name='appId']:checked").each(function(i, d){
						if(i > 0){
							appIds += ",";
						}
						appIds += $(this).val();
					});
					$("#_appIds").val(appIds);
					
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/userApp/allocateSave", $.formJson('_editForm'),function(d) {
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
		});
	});
</script>
