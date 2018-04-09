<%@ page language="java" pageEncoding="utf-8"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="应用"/>
</jsp:include>

<div class="page-header">
	<h1>
		应用列表
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
									<label class="control-label" for="form-field-1"> 名称： </label>
									<input name="name" type="text" class="form-data input-medium search-data">
								</label>
								<!-- 
								<button id="_search" type="button" class="btn btn-info btn-sm">
									<i class="ace-icon fa fa-search bigger-110"></i>搜索
								</button>
								 -->
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
    			url : "${_path}/admin/app/list",
    			formId : "_form",
				tools : [
					{text : '新增', clazz : 'btn-info', icon : 'fa fa-plus-circle blue', permission : '/admin/app/edit', handler : function(){
						$.aceRedirect("${_path}/admin/app/edit");
					}},
					{text : '禁用', clazz : 'btn-warning', icon : 'fa fa-lock orange', permission : '/admin/app/enable', handler : function(){
						$table.ajaxEnable({url : "${_path}/admin/app/enable"}, false);
					}},
					{text : '启用', clazz : 'btn-success', icon : 'fa fa-unlock green', permission : '/admin/app/enable', handler : function(){
						$table.ajaxEnable({url : "${_path}/admin/app/enable"}, true);
					}},
					{text : '删除', clazz : 'btn-danger', icon : 'fa fa-trash-o red', permission : '/admin/app/delete', handler : function(){
						$table.ajaxDelete({
							confirm : "删除应用会关联删除对应的权限，确认要删除?", 
							url : "${_path}/admin/app/delete"
						});
					}}
				],
				columns : [
			        {field:'id', hide : true},
			        {field:'isEnable', hide : true},
			        {field:'name', title:'名称', align:'left'},
			        {field:'code', title:'编码', align:'left', mobileHide : true},
			        {field:'sort', title:'排序', mobileHide : true},
			        {field:'isEnableStr', title:'是否启用', replace : function (d){
				        if(d.isEnable)
				        	return "<span class='label label-sm label-success'>" + d.isEnableStr + 	"</span>";
			        	else
			        		return "<span class='label label-sm label-warning'>" + d.isEnableStr + "</span>";
			        }},
			        {field:'createTime', title:'创建时间', mobileHide : true}
				],
				operate : [
					{text : '修改', clazz : 'blue', icon : 'fa fa-pencil', permission : '/admin/app/edit', handler : function(d, i){
						$.aceRedirect("${_path}/admin/app/edit?id=" + d.id);
					}},
					{text : '禁用', clazz : 'orange', icon : 'fa fa-lock', permission : '/admin/app/enable', 
						handler : function(){
							$table.ajaxEnable({url : "${_path}/admin/app/enable"}, false);
						},
						show : function(d){
							return d.isEnable;
						}
					},
					{text : '启用', clazz : 'green', icon : 'fa fa-unlock', permission : '/admin/app/enable', 
						handler : function(){
							$table.ajaxEnable({url : "${_path}/admin/app/enable"}, true);
						},
						show : function(d){
							return !d.isEnable;
						}
					},
					{text : '删除', clazz : 'red', icon : 'fa fa-trash-o', permission : '/admin/app/delete', handler : function(d, i){
						$table.ajaxDelete({
							confirm : "删除应用会关联删除对应的权限，确认要删除?", 
							url : "${_path}/admin/app/delete"
						});
					}}
				],
				after : function(){
					// 权限处理
					$.permission();
				}
			});
			
			// 搜索
			$(".search-data").keyup(function () { 
				$table.search();
			});
		});
	});
</script>
