<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="权限"/>
</jsp:include>
<link rel="stylesheet" href="${_staticPath}/custom/zTree/css/metroStyle/metroStyle.css?v=1" />
<link rel="stylesheet" href="${_staticPath}/custom/zTree/css/metroStyle/metroStyle.custom.css" />
<style type="text/css">
	.ztree li span.button.switch.level0 {visibility:hidden; width:1px;}
	.ztree li ul.level0 {padding:0; background:none;}
</style>
	
<div class="page-header">
	<h1>
		权限管理
	</h1>
</div><!-- /.page-header -->

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
									<select id="_searchAppId" name="appId">
										<c:forEach var="item" items="${appList}">
											<option value="${item.id}">${item.name}</option>
										</c:forEach>
									</select>
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

<div class="row">
	<div class="col-xs-12">
		<ul id="_tree" class="ztree" style="width:560px; overflow:auto;"></ul>
	</div><!-- /.col -->
	
	<a id="my-modal-a" href="#my-modal" role="button" class="bigger-125 bg-primary white" style="display: none;" data-toggle="modal"></a>

	<div id="my-modal" class="modal fade" tabindex="-1">
		<div class="modal-dialog">
			<div class="modal-content">
				<div class="modal-header">
					<button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
					<h3 class="smaller lighter blue no-margin">权限信息</h3>
				</div>

				<div class="modal-body">
					<form id="_editForm" class="form-horizontal" role="form"
						validate="true">
						<input type="hidden" id="_id" name="id">
						<input type="hidden" id="_parentId" name="parentId">
			
						<div class="form-group">
							<label for="_appId" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>应用</label>
			
							<div class="col-sm-3">
								<select id="_appId" class="form-control help-validate"
									required="true">
									<c:forEach var="item" items="${appList}">
										<option value="${item.id}">${item.name}</option>
									</c:forEach>
								</select>
								<input id="_hidden_appId" type="hidden" name="appId"/>
							</div>
						</div> 
						
						<div class="form-group">
							<label for="_name" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>名称</label>
			
							<div class="col-sm-9">
								<div class="clearfix help-validate">
									<input id="_name" name="name" type="text" class="form-data col-xs-10 col-sm-5" placeholder="名称"
										required="true" maxlength="64"/>
								</div>
							</div>
							
						</div>
						
						<div class="form-group">
							<label for="_icon" class="col-sm-3 control-label no-padding-right">图标</label>
			
							<div class="col-sm-9">
								<div class="clearfix help-validate">
									<input id="_icon" name="icon" type="text" class="form-data col-xs-10 col-sm-5" placeholder="图标"
										maxlength="100"/>&nbsp;
									<i id="_icon_fa"></i>
								</div>
							</div>
							
						</div>
						
						<div class="form-group">
							<label for="_sort" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>排序</label>
			
							<div class="col-sm-9">
								<div class="clearfix help-validate">
									<input id="_sort" name="sort" type="text" class="form-data col-xs-10 col-sm-5" placeholder="排序"
										required="true" vtype="integer" min="1" max="9999"/>
								</div>
							</div>
							
						</div>
					
						<div class="form-group">
							<label for="_url" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>URL</label>
			
							<div class="col-sm-9">
								<div class="clearfix help-validate">
									<input id="_url" name="url" type="text" class="form-data col-xs-10 col-sm-9" placeholder="URL"
										required="true" maxlength="64"/>
								</div>
							</div>
							
						</div>
						
						<div class="form-group">
							<label for="_isMenu" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>是否菜单</label>
			
							<div class="col-xs-12 col-sm-9">
								<div class="clearfix help-validate">
									<label class="line-height-1 blue">
										<input name="isMenu" value="true" type="radio" class="ace"/>
										<span class="lbl"> 是</span>
									</label>
									<label class="line-height-1 blue">
										<input name="isMenu" value="false" type="radio" class="ace"/>
										<span class="lbl"> 否</span>
									</label>
								</div>
							</div>
							
						</div>
						
						<div class="form-group">
							<label for="_isEnable" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>是否启用</label>
			
							<div class="col-xs-12 col-sm-9">
								<div class="clearfix help-validate">
									<label class="line-height-1 blue">
										<input name="isEnable" value="true" type="radio" class="ace"/>
										<span class="lbl"> 是</span>
									</label>
									<label class="line-height-1 blue">
										<input name="isEnable" value="false" type="radio" class="ace"/>
										<span class="lbl"> 否</span>
									</label>
								</div>
							</div>
							
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
</div><!-- /.row -->

<script type="text/javascript">
	scripts.push(
			"${_staticPath}/custom/zTree/js/jquery.ztree.core-3.5.min.js",
			"${_staticPath}/custom/zTree/js/jquery.ztree.excheck-3.5.min.js");
		
	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
    		var setting = {
	            view: {
	            	addDiyDom: addDiyDom,
	                selectedMulti: false
	            },
	            async: {
					enable: true,
					contentType: "application/x-www-form-urlencoded",
					type: "get",
					otherParam: $.formJson("_form"),
					dataType: "text",
					url: "${_path}/admin/permission/nodes"
				},
	            check: {
	                enable: false
	            },
	            data: {
	                simpleData: {
	                    enable: true
	                }
	            },
	            callback: {
	            	onAsyncSuccess: zTreeOnAsyncSuccess
	            }
	        };
	        
	        //树菜单初始化
	        var treeObj = $.fn.zTree.init($("#_tree"), setting);
	        
	        //弹出层初始化
	        $('.modal.aside').ace_aside();
		
			$(document).one('ajaxloadstart.page', function(e) {
				//in ajax mode, remove before leaving page
				$('.modal.aside').remove();
				$(window).off('.aside');
			});
			
			//显示icon图标效果
			$("#_icon").bind("blur", function(){
				$("#_icon_fa").attr("class", "menu-icon fa "+$(this).val());
			});
			
			//点击保存按钮
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/permission/save", $.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							$.gritter.add({
								text: d.message,
								sticky: false,
								time: '1000'
							});
							$("#_editForm")[0].reset();
							reloadTree();
						}
			        },'json');
				}else{
					return false;
				}
			});
			
			// 取消
			$("#_cancel").click(function(){
				$("#_editForm")[0].reset();
				reloadTree();
			});
			
			// 回车绑定
			$(".form-data").bind('keypress',function(event){
                if(event.keyCode == "13"){
                	event.preventDefault();
                	$("#_submit").click();
                }
            });
	        
	        //在节点上固定显示用户自定义控件
	        var IDMark_A = "_a";
	        function addDiyDom(treeId, treeNode) {
	       		var aObj = $("#" + treeNode.tId + IDMark_A);
	       		if(treeNode.isMenu){
	       			aObj.css("color","#438eb9");
	       		}
				var editStr = "&nbsp;<a id='add_" + treeNode.id + "' class='blue' href='javascript:void(0)' title='添加'>		<i class='ace-icon fa fa-plus bigger-110'></i>	</a>";
	       		if(treeNode.id) {
					editStr += "&nbsp;<a id='edit_" + treeNode.id + "' class='green' permission='/admin/permission/save' href='javascript:void(0)' title='修改'>		<i class='ace-icon fa fa-pencil bigger-110'></i>	</a>";
					editStr += "&nbsp;<a id='delete_" + treeNode.id + "' class='red' permission='/admin/permission/delete' href='javascript:void(0)' title='删除'>		<i class='ace-icon fa fa-trash-o bigger-110'></i>	</a>";
				}
				aObj.after(editStr);
				var addBtn = $("#add_" + treeNode.id);
				if(addBtn){
					addBtn.bind("click", function(){
						setForm(treeNode,'add');
						$("#my-modal-a").click();
						$("#_name").focus();
					});
				}
				var editBtn = $("#edit_" + treeNode.id);
				if(editBtn){
					editBtn.bind("click", function(){
						setForm(treeNode,'update');
						$("#my-modal-a").click();
						$("#_name").focus();
					});
				}
				var deleteBtn = $("#delete_" + treeNode.id);
				if(deleteBtn){
					deleteBtn.bind("click", function(){
						bootbox.confirm( "会删除该权限所有子权限，确认要删除吗?",function(result) {
							if(result) {
								$.post("${_path}/admin/permission/delete", {"id":treeNode.id, "appId":$("#_searchAppId option:selected").val()},function(d) {
									if(d){
										if(d.code == 1){
											$.gritter.add({
												text: d.message,
												sticky: false,
												time: '1000'
											});
										}
										reloadTree();
									}
						        },'json');
							}
						});
					});
				}
	        };
	        
			//树加载成功后，全部展开
			function zTreeOnAsyncSuccess(event, treeId, treeNode, msg) {
			    treeObj.expandAll(true);
			 	// 权限处理
				$.permission();
			};
			
			//增加、修改时填充表单
			function setForm(treeNode, flag){
				$("#_editForm")[0].reset();
				$("#_icon_fa").removeAttr("class");
				$("#_appId").val(treeNode.appId);
				$("#_hidden_appId").val(treeNode.appId);
				if(flag == 'add'){
					$("#_parentId").val(treeNode.id);
					$("#_sort").val(1);
					$("#_id").val("");
					$("input[type=radio][name=isMenu][value=false]").prop('checked', false);
					$("input[type=radio][name=isMenu][value=true]").prop('checked', true);
					$("input[type=radio][name=isEnable][value=false]").prop('checked', false);
					$("input[type=radio][name=isEnable][value=true]").prop('checked', true);
				}else if(flag == 'update'){
					$("#_id").val(treeNode.id);
					$("#_parentId").val(treeNode.pId);
					$("#_name").val(treeNode.name);
					$("#_icon").val(treeNode.permissionIcon);
					$("#_icon_fa").attr("class", "menu-icon fa "+treeNode.permissionIcon);
					$("#_url").val(treeNode.urlStr);
					$("#_sort").val(treeNode.sort);
					$("input[type=radio][name=isMenu][value!=" + treeNode.isMenu + "]").prop('checked', false);
					$("input[type=radio][name=isMenu][value=" + treeNode.isMenu + "]").prop('checked', true); 
					$("input[type=radio][name=isEnable][value!=" + treeNode.isEnable + "]").prop('checked', false);
					$("input[type=radio][name=isEnable][value=" + treeNode.isEnable + "]").prop('checked', true);
				}
				$("#_appId").attr("disabled","disabled");;
			}
			// 搜索
			$("#_searchAppId").change(function () { 
				reloadTree();
           	});
           	
           	function reloadTree(){
           		setting.async.otherParam = $.formJson("_form");
           		treeObj = $.fn.zTree.init($("#_tree"), setting);
           	}
		});
	});
</script>
