<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="机构"/>
</jsp:include>

<div class="row">
	<div class="col-xs-12">
		<div class="row">
			<div class="col-xs-12">
				<form id="_form" />
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
				url : "${_path}/admin/office/list",
				formId : "_form",
				tools : [
					{text : '新增', clazz : 'btn-info', icon : 'fa fa-plus-circle blue', permission : '/admin/office/edit', handler : function(){
						$.aceRedirect("${_path}/admin/office/edit");
					}},
					{text : '禁用', clazz : 'btn-warning', icon : 'fa fa-lock orange', permission : '/admin/office/enable', handler : function(){
						$table.ajaxEnable({url : "${_path}/admin/office/enable"}, false);
					}},
					{text : '启用', clazz : 'btn-success', icon : 'fa fa-unlock green', permission : '/admin/office/enable', handler : function(){
						$table.ajaxEnable({url : "${_path}/admin/office/enable"}, true);
					}},
					{text : '删除', clazz : 'btn-danger', icon : 'fa fa-trash-o red', permission : '/admin/office/delete', handler : function(){
						$table.ajaxDelete({url : "${_path}/admin/office/delete"});
					}}
				],
				columns : [
					{field:'id', hide : true},
					{field:'isEnable', hide : true},
					{field:'name', title:'名称', align:'left'},
					{field:'sort', title:'排序', mobileHide : true},
					{field:'isEnableStr', title:'是否启用', replace : function (d){
				        if(d.isEnable)
				        	return "<span class='label label-sm label-success'>" + d.isEnableStr + 	"</span>";
			        	else
			        		return "<span class='label label-sm label-warning'>" + d.isEnableStr + "</span>";
			        }}
				],
				operate : [
					{text : '修改', clazz : 'blue', icon : 'fa fa-pencil', permission : '/admin/office/edit', handler : function(d, i){
						$.aceRedirect("${_path}/admin/office/edit?id=" + d.id);
					}},
					{text : '禁用', clazz : 'orange', icon : 'fa fa-lock', permission : '/admin/office/enable', 
						handler : function(){
							$table.ajaxEnable({url : "${_path}/admin/office/enable"}, false);
						},
						show : function(d){
						    return d.isEnable;
						}
					},
					{text : '启用', clazz : 'green', icon : 'fa fa-unlock', permission : '/admin/office/enable', 
						handler : function(){
							$table.ajaxEnable({url : "${_path}/admin/office/enable"}, true);
						},
						show : function(d){
						    return !d.isEnable;
						}
					},
					{text : '删除', clazz : 'red', icon : 'fa fa-trash-o', permission : '/admin/office/delete', handler : function(d, i){
						$table.ajaxDelete({url : "${_path}/admin/office/delete"});
					}}
				],
				after : function(){
					// 权限处理
					$.permission();
				},
				pagination : {hide : true}
			});
		});
	});
</script>
