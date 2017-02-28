<%@ page language="java" pageEncoding="utf-8"%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="应用"/>
</jsp:include>

<div class="page-header">
	<h1>
		${empty app.id ? '添加' : '修改'}应用
	</h1>
</div>

<div class="row">
	<div class="col-xs-12">
		<form id="_editForm" class="form-horizontal" role="form">
			<input type="hidden" name="id" value="${app.id}"> 
			
			<div class="form-group">
				<label for="_name" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>名称</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_name" name="name" type="text" value="${app.name}" class="form-data col-xs-10 col-sm-5" placeholder="应用名称"
							required="true" maxlength="64"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label for="_code" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>编码</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_code" name="code" type="text" value="${app.code}" class="form-data col-xs-10 col-sm-5" placeholder="应用编码"
							required="true" ajax="{url : '${_path}/admin/app/validateCode', type : 'post', dataId : '_editForm'}" maxlength="64"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label for="_sort" class="col-sm-3 control-label no-padding-right"><span class="form-star">*</span>排序</label>

				<div class="col-sm-9">
					<div class="clearfix help-validate">
						<input id="_sort" name="sort" type="text" value="${app.sort}" class="form-data col-xs-10 col-sm-5" placeholder="排序"
							required="true" vtype="integer" min="1" max="9999"/>
					</div>
				</div>
				
			</div>
			
			<div class="form-group">
				<label class="control-label col-xs-12 col-sm-3 no-padding-right"><span class="form-star">*</span>是否启用</label>

				<div class="col-xs-12 col-sm-9">
					<div class="clearfix help-validate">
						<div>
							<label class="line-height-1 blue">
								<input name="isEnable" value="true" type="radio" class="ace" ${app.isEnable ? 'checked="checked"' : ''}/>
								<span class="lbl"> 是</span>
							</label>
						</div>
	
						<div>
							<label class="line-height-1 blue">
								<input name="isEnable" value="false" type="radio" class="ace" ${!app.isEnable ? 'checked="checked"' : ''}/>
								<span class="lbl"> 否</span>
							</label>
						</div>
					</div>
				</div>
			</div>
			
			<div class="clearfix form-actions">
				<div class="col-md-offset-3 col-md-9">
					<button id="_submit" type="button" class="btn btn-info" data-loading-text="正在提交..." permission="/app/save">
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
			
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/app/save", $.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							if(d.code == 1){
								$.aceRedirect("${_path}/admin/app");
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
				$.aceRedirect("${_path}/admin/app");
			});
			
			// 回车绑定
			$(".form-data").bind('keypress',function(event){
                if(event.keyCode == "13"){
                	event.preventDefault();
                	$("#_submit").click();
                }
            });
            
            // 权限处理
			$.permission();
		});
	});
</script>
