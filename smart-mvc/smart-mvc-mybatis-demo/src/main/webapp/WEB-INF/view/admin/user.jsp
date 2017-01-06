<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="用户"/>
</jsp:include>

<div class="page-header">
	<h1>
		用户列表
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<div class="row">
			<div class="col-xs-12">
				<div class="widget-box">
					<div class="widget-header widget-header-small">
						<h5 class="widget-title lighter">搜索栏</h5>
					</div>

					<div class="widget-body">
						<div class="widget-main">
							<form id="_form" class="form-inline">
								<label>
									<label class="control-label" for="form-field-1"> 登录名： </label>
									<input name="account" type="text" class="form-data input-medium search-data">
								</label>
							</form>
						</div>
					</div>
				</div>

				<div>
					<div class="dataTables_wrapper form-inline no-footer">
						<table id="_table" class="table table-striped table-bordered table-hover dataTable no-footer">
						</table>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<script type="text/javascript">
	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
			// 列表
    		var $table = $("#_table").table({
    			url : "${_path}/admin/user/list",
    			formId : "_form",
				tools : [
					{text : '新增', clazz : 'btn-info', icon : 'fa fa-plus-circle blue', permission : '/admin/user/edit', handler : function(){
						window.location.href = "${_path}/admin/admin#/admin/user/edit";
					}},
					{text : '删除', clazz : 'btn-danger', icon : 'fa fa-trash-o red', permission : '/admin/user/delete', handler : function(){
						$table.ajaxDelete({url : "${_path}/admin/user/delete"});
					}}
				],
				columns : [
			        {field:'id', hide : true},
			        {field:'account', title:'登录名', align:'left'}
				],
				operate : [
					{text : '修改', clazz : 'blue', icon : 'fa fa-pencil', permission : '/admin/user/edit', handler : function(d, i){
						window.location.href = "${_path}/admin/admin#/admin/user/edit?id=" + d.id;
					}},
					{text : '删除', clazz : 'red', icon : 'fa fa-trash-o', permission : '/admin/user/delete', handler : function(d, i){
						$table.ajaxDelete({url : "${_path}/admin/user/delete"});
					}}
				],
				after : function(){
					// 权限处理
					$.permission();
				}
			});
			
			//搜索
			$(".search-data").keyup(function () { 
				$table.search();
			});
		});
	});
</script>
