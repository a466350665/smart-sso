<%@ page language="java" pageEncoding="utf-8"%>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%  
    response.setHeader("Pragma", "No-cache");
    response.setHeader("Cache-Control", "no-cache");
    response.setDateHeader("Expires", 0);   
%>

<jsp:include page="../common/common.jsp">
	<jsp:param name="title" value="个人中心"/>
</jsp:include>

<!-- ajax layout which only needs content area -->
<div class="page-header">
	<h1>
		个人中心
	</h1>
</div><!-- /.page-header -->

<div class="row">
	<div class="col-xs-12">
		<!-- PAGE CONTENT BEGINS -->
		<div>
			<div id="user-profile-3" class="user-profile row">
				<div class="col-sm-offset-1 col-sm-10">

					<div class="space"></div>
					
					<form id="_editForm" class="form-horizontal"
						validate="true">
						<div class="tabbable">
							<ul class="nav nav-tabs padding-16">
								<li class="active">
									<a data-toggle="tab" href="#edit-basic">
										<i class="green ace-icon fa fa-pencil-square-o bigger-125"></i>
										基本信息
									</a>
								</li>
	
								<li>
									<a data-toggle="tab" href="#edit-password">
										<i class="blue ace-icon fa fa-key bigger-125"></i>
										修改密码
									</a>
								</li>
							</ul>
	
							<div class="tab-content profile-edit-tab-content">
								<div id="edit-basic" class="tab-pane in active">
									<h4 class="header blue bolder smaller">注册信息</h4>
	
									<div class="form-group">
										<label class="col-sm-3 control-label no-padding-right" for="form-field-email">登录名</label>
	
										<div class="col-sm-9">
											<label class="control-label"><b>${user.account}</b></label>
										</div>
									</div>
	
									<div class="space-4"></div>
	
									<div class="form-group">
										<label class="col-sm-3 control-label no-padding-right" for="form-field-website">注册时间</label>
	
										<div class="col-sm-9">
											<label class="control-label"><b><fmt:formatDate  value="${user.createTime}" pattern="yyyy-MM-dd HH:mm:ss" /></b></label>
										</div>
									</div>
	
									<div class="space"></div>
									<h4 class="header blue bolder smaller">登录信息</h4>
	
									<div class="form-group">
										<label class="col-sm-3 control-label no-padding-right" for="form-field-facebook">最后登录IP</label>
	
										<div class="col-sm-9">
											<label class="control-label"><b>${user.lastLoginIp}</b></label>
										</div>
									</div>
	
									<div class="space-4"></div>
	
									<div class="form-group">
										<label class="col-sm-3 control-label no-padding-right" for="form-field-twitter">登录总次数</label>
	
										<div class="col-sm-9">
											<label class="control-label"><b>${user.loginCount}</b></label>
										</div>
									</div>
	
									<div class="space-4"></div>
	
									<div class="form-group">
										<label class="col-sm-3 control-label no-padding-right" for="form-field-gplus">最后登录时间</label>
	
										<div class="col-sm-9">
											<label class="control-label"><b><fmt:formatDate  value="${user.lastLoginTime}" pattern="yyyy-MM-dd HH:mm:ss" /></b></label>
										</div>
									</div>
								</div>
	
								<div id="edit-password" class="tab-pane">
									<div class="space-10"></div>

									<div class="form-group">
										<label class="col-sm-3 control-label no-padding-right" for="form-field-pass1">新密码</label>

										<div class="col-sm-9">
											<div class="help-validate">
												<input id="_newPassword" name="newPassword" type="password" value=""  class="form-data"
													required="true" minlength = '6' maxlength = '16'/>
											</div>
										</div>
									</div>

									<div class="space-4"></div>

									<div class="form-group">
										<label class="col-sm-3 control-label no-padding-right" for="form-field-pass2">确认密码</label>

										<div class="col-sm-9">
											<div class="help-validate">
												<input id="_confirmPassword" name="confirmPassword" type="password" value=""  class="form-data"
													required="true" equalsTo="_newPassword:两次输入的密码不一致" minlength = '6' maxlength = '16'/>
											</div>
										</div>
									</div>
									
									<div class="clearfix form-actions">
										<div class="col-md-offset-3 col-md-9">
											<button id="_submit" type="button" class="btn btn-info" data-loading-text="正在提交...">
												<i class="ace-icon fa fa-check bigger-110"></i>
												保存
											</button>
			
											&nbsp; &nbsp;
											<button id="_reset" class="btn" type="reset">
												<i class="ace-icon fa fa-undo bigger-110"></i>
												重置
											</button>
										</div>
									</div>
								</div>
							</div>
						</div>
					</form>
				</div><!-- /.span -->
			</div><!-- /.user-profile -->
		</div>

		<!-- PAGE CONTENT ENDS -->
	</div><!-- /.col -->
</div><!-- /.row -->

<!--[if lte IE 8]>
  <script src="${_staticPath}/assets/js/excanvas.js"></script>
<![endif]-->
<script type="text/javascript">
	$('.page-content-area').ace_ajax('loadScripts', scripts, function() {
		jQuery(function($) {
			// 提交
			$("#_submit").click(function(){
				if($('#_editForm').validate()){
					var btn = $(this);
					btn.button('loading');
					$.post("${_path}/admin/profile/savePassword", $.formJson('_editForm'),function(d) {
						if(d){
							btn.button('reset');
							$("#_reset").click();
							$.gritter.add({text: d.message});
						}
			        },'json');
				}
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
