<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="${tableComment!''}"/>
</jsp:include>

<div class="page-header">
	<h1>
		${tableComment!''}
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
								<button id="_search" type="button" class="btn btn-info btn-sm">
									<i class="ace-icon fa fa-search bigger-110"></i>搜索
								</button>
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
    			url : "${path!''}/admin/${_model!''}/list",
    			formId : "_form",
				tools : [
					{text : '新增', clazz : 'btn-info', icon : 'fa fa-plus-circle blue', permission : '/admin/${_model!''}/edit', handler : function(){
						$.aceRedirect("${path!''}/admin/${_model!''}/edit");
					}},
					<#if containEnable>
					{text : '禁用', clazz : 'btn-warning', icon : 'fa fa-lock orange', permission : '/admin/${_model!''}/enable', handler : function(){
						$table.ajaxEnable({url : "${path!''}/admin/${_model!''}/enable"}, false);
					}},
					{text : '启用', clazz : 'btn-success', icon : 'fa fa-unlock green', permission : '/admin/${_model!''}/enable', handler : function(){
						$table.ajaxEnable({url : "${path!''}/admin/${_model!''}/enable"}, true);
					}},
					</#if>
					{text : '删除', clazz : 'btn-danger', icon : 'fa fa-trash-o red', permission : '/admin/${_model!''}/delete', handler : function(){
						$table.ajaxDelete({url : "${path!''}/admin/${_model!''}/delete"});
					}}
				],
				columns : [
					{field:'id', hide : true},
				<#if containEnable>
					{field:'${enableName!''}', hide : true},
				</#if>
				<#list fieldList as field>
			        <#if field.fieldName == enableName>
				        <#if containEnable>
			        {field:'${enableName!''}Str', title:'是否启用', replace : function (d){
				        if(d.${enableName!''})
				        	return "<span class='label label-sm label-success'>" + d.${enableName!''}Str + 	"</span>";
			        	else
			        		return "<span class='label label-sm label-warning'>" + d.${enableName!''}Str + "</span>";
			        }}<#if field_has_next>,</#if>
				        </#if>
			        <#else>
			        {field:'${field.fieldName}', title:'${field.description}', mobileHide : true}<#if field_has_next>,</#if>
			        </#if>
			    </#list>
				],
				operate : [
					{text : '修改', clazz : 'blue', icon : 'fa fa-pencil', permission : '/admin/${_model!''}/edit', handler : function(d, i){
						$.aceRedirect("${path!''}/admin/${_model!''}/edit?id=" + d.id);
					}},
					<#if containEnable>
					{text : '禁用', clazz : 'orange', icon : 'fa fa-lock', permission : '/admin/${_model!''}/enable', 
						handler : function(){
							$table.ajaxEnable({url : "${path!''}/admin/${_model!''}/enable"}, false);
						},
						show : function(d){
						    return d.${enableName!''};
						}
					},
					{text : '启用', clazz : 'green', icon : 'fa fa-unlock', permission : '/admin/${_model!''}/enable', 
						handler : function(){
							$table.ajaxEnable({url : "${path!''}/admin/${_model!''}/enable"}, true);
						},
						show : function(d){
						    return !d.${enableName!''};
						}
					},
					</#if>
					{text : '删除', clazz : 'red', icon : 'fa fa-trash-o', permission : '/admin/${_model!''}/delete', handler : function(d, i){
						$table.ajaxDelete({url : "${path!''}/admin/${_model!''}/delete"});
					}}
				],
				after : function(){
					// 权限处理
					$.permission();
					// 金额格式化
					$.decimalFormat();
				}
			});
			
			// 搜索
			$("#_search").click(function () {
           		$table.search();
           	});
           	
           	// 回车绑定
			$(".form-data").bind('keypress',function(event){
                if(event.keyCode == "13"){
                	event.preventDefault();
                	$table.search();
                }
            });
		});
	});
</script>
