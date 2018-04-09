<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="角色授权"/>
</jsp:include>
<link rel="stylesheet" href="${_staticPath}/custom/zTree/css/metroStyle/metroStyle.css?v=1" />
<link rel="stylesheet" href="${_staticPath}/custom/zTree/css/metroStyle/metroStyle.custom.css" />

<div class="page-header">
	<h1>
		角色授权
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<!-- PAGE CONTENT BEGINS -->
		<form id="_editForm" class="form-horizontal" role="form" 
			validate="true">
			<input name="roleId" type="hidden" value="${roleId}" required="true">
			
			<div class="form-group">
				<label for="_appId" class="col-sm-3 control-label no-padding-right">应用</label>

				<div class="col-sm-5">
					<select id="_appId" name="appId" class="form-control help-validate"
						required="true">
						<c:forEach var="item" items="${appList}">
							<option value="${item.id}">${item.name}</option>
						</c:forEach>
					</select>
				</div>
			</div>
			
			<div class="form-group">
				<label class="col-sm-3 control-label no-padding-right">权限</label>
				<div class="col-sm-3">
					<ul id="_tree" class="ztree" style="width:560px; overflow:auto;"></ul>
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


<!--[if lte IE 8]>
  <script src="${_staticPath}/assets/js/excanvas.js"></script>
<![endif]-->
<script type="text/javascript">
	scripts.push(
		"${_staticPath}/custom/zTree/js/jquery.ztree.core-3.5.min.js",
		"${_staticPath}/custom/zTree/js/jquery.ztree.excheck-3.5.min.js");

	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
			var setting = {
	            view: {
	                selectedMulti: true
	            },
	            async: {
					enable: true,
					contentType: "application/x-www-form-urlencoded",
					otherParam: $.extend($.formJson('_editForm'), {"isEnable" : true}),
					type: "get",
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
			
			//树加载成功后，全部展开
			function zTreeOnAsyncSuccess(event, treeId, treeNode, msg) {
			    treeObj.expandAll(true);
			};
			
			function zTreeOnClick(event, treeId, treeNode){
				treeObj.checkNode(treeNode, null, true);
			}
			
			var treeObj = $.fn.zTree.init($("#_tree"), setting);
			
			$("#_appId").change(function () {
	        	setting.async.otherParam = $.formJson('_editForm');
           		treeObj = $.fn.zTree.init($("#_tree"), setting);
           	});
			
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
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
						permissionIds = permissionIds.substring(0, permissionIds.length - 1);
					}
					
					$.post("${_path}/admin/rolePermission/save", $.extend($.formJson('_editForm'), {"isEnable" : true, 'permissionIds' : permissionIds}), function(d) {
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
		});
	});
</script>
