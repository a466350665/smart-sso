<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="角色"/>
</jsp:include>

<div class="page-header">
	<h1>
		角色列表
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
									<label class="control-label" for="form-field-1"> 角色名： </label>
									<input name="name" type="text" class="form-data input-medium search-data">
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
    			url : "${_path}/admin/role/list",
    			formId : "_form",
				tools : [
					{text : '新增', clazz : 'btn-info', icon : 'fa fa-plus-circle blue', permission : '/admin/role/edit', handler : function(){
						$.aceRedirect("${_path}/admin/role/edit");
					}},
					{text : '禁用', clazz : 'btn-warning', icon : 'fa fa-lock orange', permission : '/admin/role/enable', handler : function(){
						$table.ajaxEnable({url : "${_path}/admin/role/enable"}, false);
					}},
					{text : '启用', clazz : 'btn-success', icon : 'fa fa-unlock green', permission : '/admin/role/enable', handler : function(){
						$table.ajaxEnable({url : "${_path}/admin/role/enable"}, true);
					}},
					{text : '删除', clazz : 'btn-danger', icon : 'fa fa-trash-o red', permission : '/admin/role/delete', handler : function(){
						$table.ajaxDelete({
							confirm : "删除角色会影响关联的用户及权限，确认要删除?",
							url : "${_path}/admin/role/delete"
						});
					}},
					{text : '角色授权', clazz : 'btn-default', icon : 'fa fa-cog grey', permission : '/admin/role/allocate', handler : function(){
						if($table.validateSelected(true)){
							$.aceRedirect("${_path}/admin/rolePermission?roleId=" + $table.getSelectedItemKeys("id"));
						}
					}}
				],
				columns : [
			        {field:'id', hide : true},
			        {field:'isEnable', hide : true},
			        {field:'appId', hide : true},
			        {field:'name', title:'角色名', align:'left'},
			        {field:'description', title:'描述', align:'left', mobileHide : true},
			        {field:'isEnableStr', title:'是否启用', replace : function (d){
				        if(d.isEnable)
				        	return "<span class='label label-sm label-success'>" + d.isEnableStr + 	"</span>";
			        	else
			        		return "<span class='label label-sm label-warning'>" + d.isEnableStr + "</span>";
			        }}
				],
				operate : [
					{text : '修改', clazz : 'blue', icon : 'fa fa-pencil', permission : '/admin/role/edit', handler : function(d, i){
						$.aceRedirect("${_path}/admin/role/edit?id=" + d.id);
					}},
					{text : '禁用', clazz : 'orange', icon : 'fa fa-lock', permission : '/admin/role/enable', 
						handler : function(){
							$table.ajaxEnable({url : "${_path}/admin/role/enable"}, false);
						},
						show : function(d){
							return d.isEnable;
						}
					},
					{text : '启用', clazz : 'green', icon : 'fa fa-unlock', permission : '/admin/role/enable', 
						handler : function(){
							$table.ajaxEnable({url : "${_path}/admin/role/enable"}, true);
						},
						show : function(d){
							return !d.isEnable;
						}
					},
					{text : '删除', clazz : 'red', icon : 'fa fa-trash-o', permission : '/admin/role/delete', handler : function(d, i){
						$table.ajaxDelete({
							confirm : "删除角色会影响关联的权限，确认要删除?",
							url : "${_path}/admin/role/delete"
						});
					}},
					{text : '角色授权', clazz : 'grey', icon : 'fa fa-cog', permission : '/admin/role/allocate', handler : function(d, i){
						$.aceRedirect("${_path}/admin/rolePermission?roleId=" + d.id);
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
