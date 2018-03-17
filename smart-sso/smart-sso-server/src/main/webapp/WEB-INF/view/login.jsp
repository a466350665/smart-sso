<%@ page language="java" pageEncoding="utf-8"%>
<%  
    response.setHeader("Pragma", "No-cache");
    response.setHeader("Cache-Control", "no-cache");
    response.setDateHeader("Expires", 0);   
%>
<!DOCTYPE html>
<html>
	<head>
		<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
		<meta charset="utf-8" />
		<title>登录-${_systemName}</title>
		
		<meta name="description" content="User login page" />
		<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0" />
		<link type="images/x-icon" rel="shortcut icon" href="${_staticPath}/custom/assets/favicon.ico">
		
		<script type="text/javascript">
			var url = window.location.href;
			if(url.indexOf('${loginUrl}') == -1){
				window.location.href = '${_path}${loginUrl}';
			}
		</script>

		<!-- bootstrap & fontawesome -->
		<link rel="stylesheet" href="${_staticPath}/assets/css/bootstrap.css" />
		<link rel="stylesheet" href="${_staticPath}/assets/css/font-awesome.css" />

		<!-- text fonts -->
		<link rel="stylesheet" href="${_staticPath}/assets/css/ace-fonts.css" />

		<!-- ace styles -->
		<link rel="stylesheet" href="${_staticPath}/assets/css/ace.css" />

		<!--[if lte IE 9]>
			<link rel="stylesheet" href="${_staticPath}/assets/css/ace-part2.css" />
		<![endif]-->
		<link rel="stylesheet" href="${_staticPath}/assets/css/ace-rtl.css" />

		<!--[if lte IE 9]>
		  <link rel="stylesheet" href="${_staticPath}/assets/css/ace-ie.css" />
		<![endif]-->

		<!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->

		<!--[if lt IE 9]>
		<script src="${_staticPath}/assets/js/html5shiv.js"></script>
		<script src="${_staticPath}/assets/js/respond.js"></script>
		<![endif]-->
	</head>

	<body class="login-layout">
		<div class="main-container">
			<div class="main-content">
				<div class="row">
					<div class="col-sm-10 col-sm-offset-1">
						<div class="login-container">
							<div class="center">
								<h1>
									<i class="ace-icon fa fa-leaf green"></i>
									<!-- <span class="red">XX</span> -->
									<span class="white" id="id-text2">${_systemAdminName}</span>
								</h1>
								<h4 class="blue" id="id-company-text">&copy; ${_companyName}</h4>
							</div>

							<div class="space-6"></div>

							<div class="position-relative">
								<div id="login-box" class="login-box visible widget-box no-border">
									<div class="widget-body">
										<div class="widget-main">
											<h4 class="header blue lighter bigger">
												<i class="ace-icon fa fa-coffee green"></i>
												请填写登录信息
											</h4>

											<div class="space-6"></div>

											<form id="_loginForm" action="${_path}/login" method="post"
												validate="true" vmessage="false">
												<input type="hidden" name="backUrl" value="${backUrl}" />
												<fieldset>
													<label class="block clearfix form-group">
														<span class="block input-icon input-icon-right help-validate">
															<input id="_account" name="account" type="text" class="form-control form-data" placeholder="登录名"
																required="true" minlength = '4'/>
															<i class="ace-icon fa fa-user"></i>
														</span>
													</label>

													<label class="block clearfix form-group">
														<span class="block input-icon input-icon-right help-validate">
															<input id="_password" name="password" type="password" class="form-control form-data" placeholder="密码"
																required="true" minlength = '6' maxlength = '16'/>
															<i class="ace-icon fa fa-lock"></i>
														</span>
													</label>
													
													<%--验证码--%>
													<label class="block clearfix form-group">
														<span class="block help-validate">
															<input id="_captcha" name="captcha" type="text" placeholder="验证码"
																required="true"/>
															<img alt="" src="${_path}/captcha">
														</span>
													</label>

													<div class="space"></div>

													<div class="clearfix">
														<label class="inline">
															<input id="_rememberMe" type="checkbox" class="ace" checked=""/>
															<span class="lbl"> 记住我</span>
														</label>

														<button id="_loginButton" type="button" class="width-35 pull-right btn btn-sm btn-primary">
															<i class="ace-icon fa fa-key"></i>
															<span class="bigger-110">登录</span>
														</button>
													</div>

													<div class="space-4"></div>
												</fieldset>
											</form>

											
										</div><!-- /.widget-main -->

										
									</div><!-- /.widget-body -->
								</div><!-- /.login-box -->

							</div><!-- /.position-relative -->

							<div class="navbar-fixed-top align-right">
								<br />
								&nbsp;
								<a id="btn-login-dark" href="#">Dark</a>
								&nbsp;
								<span class="blue">/</span>
								&nbsp;
								<a id="btn-login-blur" href="#">Blur</a>
								&nbsp;
								<span class="blue">/</span>
								&nbsp;
								<a id="btn-login-light" href="#">Light</a>
								&nbsp; &nbsp; &nbsp;
							</div>
						</div>
					</div><!-- /.col -->
				</div><!-- /.row -->
			</div><!-- /.main-content -->
		</div><!-- /.main-container -->

		<!-- basic scripts -->

		<!--[if !IE]> -->
		<script type="text/javascript">
			window.jQuery || document.write("<script src='${_staticPath}/assets/js/jquery.js'>"+"<"+"/script>");
		</script>

		<!-- <![endif]-->

		<!--[if IE]>
		<script type="text/javascript">
		 window.jQuery || document.write("<script src='${_staticPath}/assets/js/jquery1x.js'>"+"<"+"/script>");
		</script>
		<![endif]-->
		
		<script type="text/javascript">
			if('ontouchstart' in document.documentElement) document.write("<script src='${_staticPath}/assets/js/jquery.mobile.custom.js'>"+"<"+"/script>");
		</script>
		
		<script type="text/javascript" src="${_staticPath}/custom/jquery.cookie.js"></script>
		<script type="text/javascript" src="${_staticPath}/custom/jquery.form.min.js"></script>
		<script type="text/javascript" src="${_staticPath}/custom/jquery.validate-2.0.min.js"></script>
		<script type="text/javascript" src="${_staticPath}/custom/jquery.validate-2.0.custom.min.js"></script>

		<!-- inline scripts related to this page -->
		<script type="text/javascript">
			jQuery(function($) {
			    
				//验证是否存在错误消息
				var fail = '${errorMessage}';
				if(fail != null && fail != ''){
					alert(fail);
				}
            	
				//判断之前是否有设置cookie
				if($.cookie('_account') != undefined){
					$("#_account").val($.cookie('_account'));
					
					$("#_password").focus();
					$("#_rememberMe").attr("checked", true);
				}
				else{
					$("#_account").focus();
					$("#_rememberMe").attr("checked", false);
				}
			 	
				$('#btn-login-dark').on('click', function(e) {
					$('body').attr('class', 'login-layout');
					$('#id-text2').attr('class', 'white');
					$('#id-company-text').attr('class', 'blue');
					
					e.preventDefault();
			 	});
			 	$('#btn-login-light').on('click', function(e) {
					$('body').attr('class', 'login-layout light-login');
					$('#id-text2').attr('class', 'grey');
					$('#id-company-text').attr('class', 'blue');
					
					e.preventDefault();
			 	});
			 	$('#btn-login-blur').on('click', function(e) {
					$('body').attr('class', 'login-layout blur-login');
					$('#id-text2').attr('class', 'white');
					$('#id-company-text').attr('class', 'light-blue');
					
					e.preventDefault();
			 	});
			 	
				$("#_loginButton").click(function() {
					if($('#_loginForm').validate()){

						if($('#_rememberMe:checked').length > 0){
							$.cookie('_account', $("#_account").val());
						} 
						else{
							$.removeCookie('_account');
						}

						$('#_loginForm').submit();
					}
				});

				// 回车绑定
				$(".form-data").bind('keypress', function(event) {
					if(event.keyCode == "13"){
						event.preventDefault();
						$("#_loginButton").click();
					}
				});
			});
		</script>
	</body>
</html>
