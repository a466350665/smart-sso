<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="角色"/>
</jsp:include>
<link rel="stylesheet" href="${_staticPath}/custom/zTree/css/metroStyle/metroStyle.css?v=1" />
<link rel="stylesheet" href="${_staticPath}/custom/zTree/css/metroStyle/metroStyle.custom.css" />

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
									<label class="control-label" for="form-field-1"> 应用： </label>
									<select id="_appId" name="appId">
										<c:forEach var="item" items="${appList}">
											<option value="${item.id}" ${(item.id eq appId)?'selected="selected"':''}>${item.name}</option>
										</c:forEach>
									</select>
								</label>
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
				
				<form id="_allocateForm" role="form">
					<input id="_appId1" name="appId" type="hidden"/>
					<input id="_roleId" name="roleId" type="hidden"/>
					<input id="_permissionId" name="permissionIds" type="hidden"/>
				</form>
				<a id="my-modal-a" href="#my-modal" role="button" class="bigger-125 bg-primary white" style="display: none;" data-toggle="modal"></a>
				
				<div id="my-modal" class="modal fade" tabindex="-1">
					<div class="modal-dialog">
						<div class="modal-content">
							<div class="modal-header">
								<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
								<h3 class="smaller lighter blue no-margin">角色授权</h3>
							</div>
			
							<div class="modal-body">
								<form class="form-horizontal">
									<div class="form-group">
										<div class="col-xs-12">
											<ul id="_tree" class="ztree" style="width:560px; overflow:auto;"></ul>
										</div><!-- /.col -->
									</div>
								</form>
							</div>
			
							<div class="modal-footer">
								<button id="_cancel" class="btn btn-sm btn-danger pull-right" data-dismiss="modal" type="reset" >
									<i class="ace-icon fa fa-times"></i>
									关闭
								</button>
								<button id="_submit" class="btn btn-sm btn-success pull-right" data-dismiss="modal"  data-loading-text="正在提交...">
									<i class="ace-icon fa fa-check"></i>
									保存
								</button>
							</div>
						</div><!-- /.modal-content -->
					</div><!-- /.modal-dialog -->
				</div>
			</div>
		</div>
	</div>
</div>

<script type="text/javascript">
	scripts.push(
			"${_staticPath}/custom/zTree/js/jquery.ztree.core-3.5.min.js",
			"${_staticPath}/custom/zTree/js/jquery.ztree.excheck-3.5.min.js");
	
	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
			// 列表
    		var $table = $("#_table").table({
    			url : "${_path}/admin/role/list",
    			formId : "_form",
				tools : [
					{text : '新增', clazz : 'btn-info', icon : 'fa fa-plus-circle blue', permission : '/admin/role/edit', handler : function(){
						$.aceRedirect("${_path}/admin/role/edit?appId=" + $('#_appId').val());
					}},
					{text : '禁用', clazz : 'btn-warning', icon : 'fa fa-lock orange', permission : '/admin/role/enable', handler : function(){
						$table.ajaxEnable({url : "${_path}/admin/role/enable"}, false);
					}},
					{text : '启用', clazz : 'btn-success', icon : 'fa fa-unlock green', permission : '/admin/role/enable', handler : function(){
						$table.ajaxEnable({url : "${_path}/admin/role/enable"}, true);
					}},
					{text : '删除', clazz : 'btn-danger', icon : 'fa fa-trash-o red', permission : '/admin/role/delete', handler : function(){
						$table.ajaxDelete({
							confirm : "删除角色会影响关联的管理员及权限，确认要删除?",
							url : "${_path}/admin/role/delete"
						});
					}},
					{text : '角色授权', clazz : 'btn-default', icon : 'fa fa-cog grey', permission : '/admin/role/allocate', handler : function(){
						$("#_roleId").val($table.getSelectedItemKeys("id"));
						$("#_appId1").val($table.getSelectedItemKeys("appId"));
						$table.ajax({
							url : "${_path}/admin/role/allocate",
							type : "get",
							selectOne : true,
							data : {"roleId":$table.getSelectedItemKeys("id")},
							success : function(d) {
								if(d){
									treeObj.checkAllNodes(false);
									var data = d.data;
									for(var i=0; i<data.length; i++){
										var node = treeObj.getNodeByParam("id", data[i].permissionId, null);
										if(node){
											treeObj.checkNode(node, true);
										}
									}
								}
								$("#my-modal-a").click();
					        }
						});
						
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
						$("#_roleId").val(d.id);
						$("#_appId1").val(d.appId);
						$.ajax({
					    	url : "${_path}/admin/role/allocate", 
					    	type : "get",
					    	data : {"roleId":d.id},
					    	dataType : 'json',
					    	success : function(d) {
								if(d){
									treeObj.checkAllNodes(false);
									var data = d.data;
									for(var i=0; i<data.length; i++){
										var node = treeObj.getNodeByParam("id", data[i].permissionId, null);
										if(node){
											treeObj.checkNode(node, true);
										}
									}
								}
					        }
				        });
						$("#my-modal-a").click();
					}}
				],
				after : function(){
					// 权限处理
					$.permission();
				}
			});
			
    		var setting = {
	            view: {
	                selectedMulti: true
	            },
	            async: {
					enable: true,
					contentType: "application/x-www-form-urlencoded",
					type: "get",
					otherParam: {"appId" : $("#_appId option:selected").val(), "isEnable" : true},
					dataType: "text",
					url: "${_path}/admin/permission/nodes"
				},
	            check: {
	                enable: true,
	                chkStyle: "checkbox",
	                chkboxType: { "Y": "ps", "N": "ps" }
	            },
	            data: {
	                simpleData: {
	                    enable: true
	                }
	            },
	            callback: {
	            	onAsyncSuccess: zTreeOnAsyncSuccess,
	            	onClick: zTreeOnClick
	            }
	        };
	        
	        //树菜单初始化
	        var treeObj = $.fn.zTree.init($("#_tree"), setting);
	        
	        function reloadTree(){
	        	setting.async.otherParam = {"appId" : $("#_appId option:selected").val()};
           		treeObj = $.fn.zTree.init($("#_tree"), setting);
           	}
	        
			//搜索
			$(".search-data").keyup(function () { 
				$table.search();
				
				reloadTree();
			});
			$("#_appId").change(function () { 
				$table.search();
				
				reloadTree();
           	});
            
            //点击保存按钮
			$("#_submit").click(function(){
				var btn = $(this);
				btn.button('loading');
				var permissionIds = '';
				var nodes = treeObj.getCheckedNodes(true);
				if(nodes){
					for(var i=0; i<nodes.length; i++){
						if(nodes[i].id){
							permissionIds += nodes[i].id + ",";
						}
					}
					permissionIds = permissionIds.substring(0,permissionIds.length-1);
				}
				$("#_permissionId").val(permissionIds);
				$.post("${_path}/admin/role/allocateSave", $.formJson('_allocateForm'),function(d) {
					if(d){
						btn.button('reset');
						$("#_allocateForm")[0].reset();
						$.gritter.add({
							text: d.message,
							sticky: false,
							time: '1000'
						});
						
						$table.search();
						reloadTree();
					}
		        },'json');
			});
			
			// 取消
			$("#_cancel").click(function(){
				$("#_allocateForm")[0].reset();
				
				$table.search();
				reloadTree();
			});
            
			//树加载成功后，全部展开
			function zTreeOnAsyncSuccess(event, treeId, treeNode, msg) {
			    treeObj.expandAll(true);
			};
			
			function zTreeOnClick(event, treeId, treeNode){
				treeObj.checkNode(treeNode, null, true);
			}
		});
	});
</script>
