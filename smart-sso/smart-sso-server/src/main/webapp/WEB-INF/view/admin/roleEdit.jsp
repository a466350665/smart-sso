<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="用户"/>
</jsp:include>

<div class="page-header">
	<h1>
		${empty role.id ? '添加' : '修改'}角色
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<!-- PAGE CONTENT BEGINS -->
		<form id="_editForm" class="form-horizontal" role="form" 
			validate="true">
			<input type="hidden" name="id" value="${role.id}">
			
			<div class="form-group">
				<label for="_name" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>名称</label>

				<div class="col-sm-6">
					<div class="clearfix help-validate">
						<input id="_name" name="name" type="text" value="${role.name}" class="form-data col-xs-10 col-sm-5" placeholder="名称"
							required="true" maxlength="64"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label for="_sort" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>排序</label>

				<div class="col-sm-6">
					<div class="clearfix help-validate">
						<input id="_sort" name="sort" type="text" value="${role.sort}" class="form-data col-xs-10 col-sm-5" placeholder="排序"
							required="true" vtype="integer" min="1" max="9999"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label for="_description" class="col-sm-3 control-label no-padding-right">描述</label>
				
				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<div class="input-group">
							<textarea id="_description" name="description" class="autosize-transition form-control"  placeholder="描述"
								 rows="6" cols="60">${role.description}</textarea>
						</div>
					</div>
				</div>
			</div>
			
			<div class="form-group">
				<label class="control-label col-xs-12 col-sm-3 no-padding-right"><span class="form-star">*</span>是否启用</label>

				<div class="col-xs-12 col-sm-9">
					<div class="clearfix help-validate">
						<div>
							<label class="line-height-1 blue">
								<input name="isEnable" value="true" type="radio" class="ace" ${role.isEnable ? 'checked="checked"' : ''}/>
								<span class="lbl"> 是</span>
							</label>
						</div>
						<div>
							<label class="line-height-1 blue">
								<input name="isEnable" value="false" type="radio" class="ace" ${!role.isEnable ? 'checked="checked"' : ''}/>
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


<script type="text/javascript">
	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
			//焦点
			$("#_name").focus();
			
			//帮助查看
			$('[data-rel=tooltip]').tooltip({container:'body'});
			
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/role/save", $.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							if(d.code == 1){
								$.aceRedirect("${_path}/admin/role");
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
				$.aceRedirect("${_path}/admin/role");
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
