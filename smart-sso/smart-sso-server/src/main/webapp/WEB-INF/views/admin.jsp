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
		<title>${_systemName}</title>
		<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0" />
		<link type="images/x-icon" rel="shortcut icon" href="${_staticPath}/custom/assets/favicon.ico">
		
		<!--[if !IE]> -->
		<link rel="stylesheet" href="${_staticPath}/assets/css/pace.css" />
		<script data-pace-options='{ "ajax": true, "document": true, "eventLag": false, "elements": false }' src="${_staticPath}/assets/js/pace.js"></script>
		<!-- <![endif]-->

		<!-- bootstrap & fontawesome -->
		<link rel="stylesheet" href="${_staticPath}/assets/css/bootstrap.css" />
		<link rel="stylesheet" href="${_staticPath}/assets/css/font-awesome.css" />

		<!-- text fonts -->
		<link rel="stylesheet" href="${_staticPath}/assets/css/ace-fonts.css" />

		<!-- ace styles -->
		<link rel="stylesheet" href="${_staticPath}/assets/css/ace.css" class="ace-main-stylesheet" id="main-ace-style" />
		
		<!-- 自定义样式，表单多选框 -->
		<link rel="stylesheet" href="${_staticPath}/custom/assets/ace.custom.css" />

		<!--[if lte IE 9]>
			<link rel="stylesheet" href="${_staticPath}/assets/css/ace-part2.css" class="ace-main-stylesheet" />
		<![endif]-->

		<!--[if lte IE 9]>
		  <link rel="stylesheet" href="${_staticPath}/assets/css/ace-ie.css" />
		<![endif]-->

		<!-- ace settings handler -->
		<script src="${_staticPath}/assets/js/ace-extra.js"></script>

		<!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->

		<!--[if lte IE 8]>
		<script src="${_staticPath}/assets/js/html5shiv.js"></script>
		<script src="${_staticPath}/assets/js/respond.js"></script>
		<![endif]-->
	</head>

	<body class="no-skin">
		<!-- #section:basics/navbar.layout -->
		<div id="navbar" class="navbar navbar-default">
			<script type="text/javascript">
				try{ace.settings.check('navbar' , 'fixed')}catch(e){}
			</script>

			<div class="navbar-container" id="navbar-container">
				<!-- #section:basics/sidebar.mobile.toggle -->
				<button type="button" class="navbar-toggle menu-toggler pull-left" id="menu-toggler" data-target="#sidebar">
					<span class="sr-only">Toggle sidebar</span>

					<span class="icon-bar"></span>

					<span class="icon-bar"></span>

					<span class="icon-bar"></span>
				</button>

				<!-- /section:basics/sidebar.mobile.toggle -->
				<div class="navbar-header pull-left">
					<!-- #section:basics/navbar.layout.brand -->
					<a href="#" class="navbar-brand">
						<small>
							<i class="fa fa-leaf"></i>
							${_systemAdminName}
						</small>
					</a>

					<!-- /section:basics/navbar.layout.brand -->

					<!-- #section:basics/navbar.toggle -->

					<!-- /section:basics/navbar.toggle -->
				</div>

				<!-- #section:basics/navbar.dropdown -->
				<div class="navbar-buttons navbar-header pull-right" role="navigation">
					<ul class="nav ace-nav">
						<!--  -->
						<li class="grey">
							<a data-toggle="dropdown" class="dropdown-toggle" href="#">
								<i class="ace-icon fa fa-tasks"></i>
							</a>
						</li>

						<li class="purple">
							<a data-toggle="dropdown" class="dropdown-toggle" href="#">
								<i class="ace-icon fa fa-bell icon-animated-bell"></i>
							</a>
						</li>

						<li class="green">
							<a data-toggle="dropdown" class="dropdown-toggle" href="#">
								<i class="ace-icon fa fa-envelope icon-animated-vertical"></i>
							</a>
						</li>
						

						<!-- #section:basics/navbar.user_menu -->
						<li class="light-blue">
							<a data-toggle="dropdown" href="#" class="dropdown-toggle">
								<img class="nav-user-photo" src="${_staticPath}/assets/avatars/user.jpg" alt="Jason's Photo" />
								<span class="user-info">
									<small>欢迎光临,</small>
									${userName}
								</span>

								<i class="ace-icon fa fa-caret-down"></i>
							</a>

							<ul class="user-menu dropdown-menu-right dropdown-menu dropdown-yellow dropdown-caret dropdown-close">
								<li>
									<a href="#">
										<i class="ace-icon fa fa-cog"></i>
										设置
									</a>
								</li>

								<li>
									<a href="#">
										<i class="ace-icon fa fa-user"></i>
										个人资料
									</a>
								</li>

								<li class="divider"></li>

								<li>
									<a id="_btnExit" href="#">
										<i class="ace-icon fa fa-power-off"></i>
										退出
									</a>
								</li>
							</ul>
						</li>

						<!-- /section:basics/navbar.user_menu -->
					</ul>
				</div>

				<!-- /section:basics/navbar.dropdown -->
			</div><!-- /.navbar-container -->
		</div>

		<!-- /section:basics/navbar.layout -->
		<div class="main-container" id="main-container">
			<script type="text/javascript">
				try{ace.settings.check('main-container' , 'fixed')}catch(e){}
			</script>

			<!-- #section:basics/sidebar -->
			<div id="sidebar" class="sidebar                  responsive">
				<script type="text/javascript">
					try{ace.settings.check('sidebar' , 'fixed')}catch(e){}
				</script>

				<div class="sidebar-shortcuts" id="sidebar-shortcuts">
					<div class="sidebar-shortcuts-large" id="sidebar-shortcuts-large">
						<button class="btn btn-success">
							<i class="ace-icon fa fa-signal"></i>
						</button>

						<button class="btn btn-info">
							<i class="ace-icon fa fa-pencil"></i>
						</button>

						<!-- #section:basics/sidebar.layout.shortcuts -->
						<button class="btn btn-warning">
							<i class="ace-icon fa fa-users"></i>
						</button>

						<button class="btn btn-danger">
							<i class="ace-icon fa fa-cogs"></i>
						</button>

						<!-- /section:basics/sidebar.layout.shortcuts -->
					</div>

					<div class="sidebar-shortcuts-mini" id="sidebar-shortcuts-mini">
						<span class="btn btn-success"></span>

						<span class="btn btn-info"></span>

						<span class="btn btn-warning"></span>

						<span class="btn btn-danger"></span>
					</div>
				</div><!-- /.sidebar-shortcuts -->

				<ul class="nav nav-list">
				</ul><!-- /.nav-list -->

				<!-- #section:basics/sidebar.layout.minimize -->
				<div class="sidebar-toggle sidebar-collapse" id="sidebar-collapse">
					<i class="ace-icon fa fa-angle-double-left" data-icon1="ace-icon fa fa-angle-double-left" data-icon2="ace-icon fa fa-angle-double-right"></i>
				</div>

				<!-- /section:basics/sidebar.layout.minimize -->
				<script type="text/javascript">
					try{ace.settings.check('sidebar' , 'collapsed')}catch(e){}
				</script>
			</div>

			<!-- /section:basics/sidebar -->
			<div class="main-content">
				<div class="main-content-inner">
					<!-- #section:basics/content.breadcrumbs -->
					<div class="breadcrumbs" id="breadcrumbs">
						<script type="text/javascript">
							try{ace.settings.check('breadcrumbs' , 'fixed')}catch(e){}
						</script>

						<ul class="breadcrumb">
							<li>
								<i class="ace-icon fa fa-home home-icon"></i>
								<a href="#">首页</a>
							</li>
						</ul><!-- /.breadcrumb -->

						<!-- #section:basics/content.searchbox -->
						<div class="nav-search" id="nav-search">
							<form class="form-search">
								<span class="input-icon">
									<input type="text" placeholder="Search ..." class="nav-search-input" id="nav-search-input" autocomplete="off" />
									<i class="ace-icon fa fa-search nav-search-icon"></i>
								</span>
							</form>
						</div><!-- /.nav-search -->

						<!-- /section:basics/content.searchbox -->
					</div>

					<!-- /section:basics/content.breadcrumbs -->
					<div class="page-content">
						<!-- #section:settings.box -->
						<div class="ace-settings-container" id="ace-settings-container">
							<div class="btn btn-app btn-xs btn-warning ace-settings-btn" id="ace-settings-btn">
								<i class="ace-icon fa fa-cog bigger-130"></i>
							</div>

							<div class="ace-settings-box clearfix" id="ace-settings-box">
								<div class="pull-left width-50">
									<!-- #section:settings.skins -->
									<div class="ace-settings-item">
										<div class="pull-left">
											<select id="skin-colorpicker" class="hide">
												<option data-skin="no-skin" value="#438EB9">#438EB9</option>
												<option data-skin="skin-1" value="#222A2D">#222A2D</option>
												<option data-skin="skin-2" value="#C6487E">#C6487E</option>
												<option data-skin="skin-3" value="#D0D0D0">#D0D0D0</option>
											</select>
										</div>
										<span>&nbsp; 选择主题</span>
									</div>

									<!-- /section:settings.skins -->

									<!-- #section:settings.navbar -->
									<div class="ace-settings-item">
										<input type="checkbox" class="ace ace-checkbox-2" id="ace-settings-navbar" />
										<label class="lbl" for="ace-settings-navbar"> 固定导航栏</label>
									</div>

									<!-- /section:settings.navbar -->

									<!-- #section:settings.sidebar -->
									<div class="ace-settings-item">
										<input type="checkbox" class="ace ace-checkbox-2" id="ace-settings-sidebar" />
										<label class="lbl" for="ace-settings-sidebar"> 固定菜单栏</label>
									</div>

									<!-- /section:settings.sidebar -->
								</div><!-- /.pull-left -->

								<div class="pull-left width-50">
									<!-- #section:settings.breadcrumbs -->
									<div class="ace-settings-item">
										<input type="checkbox" class="ace ace-checkbox-2" id="ace-settings-breadcrumbs" />
										<label class="lbl" for="ace-settings-breadcrumbs"> 固定非内容</label>
									</div>

									<!-- /section:settings.breadcrumbs -->

									<!-- #section:settings.container -->
									<div class="ace-settings-item">
										<input type="checkbox" class="ace ace-checkbox-2" id="ace-settings-add-container" />
										<label class="lbl" for="ace-settings-add-container">
											容器显示内容
										</label>
									</div>


									<div class="ace-settings-item">
										<input type="checkbox" class="ace ace-checkbox-2" id="ace-settings-highlight" />
										<label class="lbl" for="ace-settings-highlight"> 菜单样式</label>
									</div>

								</div><!-- /.pull-left -->
							</div><!-- /.ace-settings-box -->
						</div><!-- /.ace-settings-container -->

						<!-- /section:settings.box -->
						<div class="page-content-area" data-ajax-content="true">
							<!-- ajax content goes here -->
						</div><!-- /.page-content-area -->
					</div><!-- /.page-content -->
				</div>
			</div><!-- /.main-content -->

			<div class="footer">
				<div class="footer-inner">
					<!-- #section:basics/footer -->
					<div class="footer-content">
						<span class="bigger-120">
							<span class="blue bolder">${_systemAdminName}</span>
							 &copy; 2017
						</span>

						&nbsp; &nbsp;
						<span class="action-buttons">
							<a href="#">
								<i class="ace-icon fa fa-twitter-square light-blue bigger-150"></i>
							</a>

							<a href="#">
								<i class="ace-icon fa fa-facebook-square text-primary bigger-150"></i>
							</a>

							<a href="#">
								<i class="ace-icon fa fa-rss-square orange bigger-150"></i>
							</a>
						</span>
					</div>

					<!-- /section:basics/footer -->
				</div>
			</div>

			<a href="#" id="btn-scroll-up" class="btn-scroll-up btn btn-sm btn-inverse">
				<i class="ace-icon fa fa-angle-double-up icon-only bigger-110"></i>
			</a>
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
		<script src="${_staticPath}/assets/js/bootstrap.js"></script>

		<!-- ace scripts -->
		<script src="${_staticPath}/assets/js/ace/elements.scroller.js"></script>
		<script src="${_staticPath}/assets/js/ace/elements.colorpicker.js"></script>
		<script src="${_staticPath}/assets/js/ace/elements.fileinput.js"></script>
		<script src="${_staticPath}/assets/js/ace/elements.typeahead.js"></script>
		<script src="${_staticPath}/assets/js/ace/elements.wysiwyg.js"></script>
		<script src="${_staticPath}/assets/js/ace/elements.spinner.js"></script>
		<script src="${_staticPath}/assets/js/ace/elements.treeview.js"></script>
		<script src="${_staticPath}/assets/js/ace/elements.wizard.js"></script>
		<script src="${_staticPath}/assets/js/ace/elements.aside.js"></script>
		<!-- 修改默认首页 -->
		<script id="_ace" src="${_staticPath}/custom/assets/ace.js?v" data-path="${defaultPage}"></script>
		<!-- 切换菜单处理 -->
		<script id="_ajaxContent" src="${_staticPath}/custom/assets/ace.ajax-content.js?v" data-path="${_path}"></script>
		<!-- 权限处理 -->
		<script id="_permission" src="${_staticPath}/custom/jquery.permission.min.js?v" data="${sessionUserNoPermissions}"></script>
		<script src="${_staticPath}/assets/js/ace/ace.touch-drag.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.sidebar.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.sidebar-scroll-1.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.submenu-hover.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.widget-box.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.settings.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.settings-rtl.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.settings-skin.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.widget-on-reload.js"></script>
		<script src="${_staticPath}/assets/js/ace/ace.searchbox-autocomplete.js"></script>
		<script type="text/javascript">
			jQuery(function ($) {
				$("#_btnExit").click(function(){
					window.location.href="${_path}/logout";
				});
			
				$.getJSON("${_path}/admin/admin/menu?v=" + Math.random(), function(d) {
   					$('.nav-list').append(tree(d.data));
				});
				
				var defaultPage = null;
				
				function tree(list) {
					var html = "";
					var data = null;
					for ( var i = 0; i < list.length; i++) {
						data = list[i];
						if (data.parentId == null || data.parentId == "") {
							
							html += '<li class="';
							
							if(defaultPage == null && data.url){
								if(window.location.href.indexOf("#") < 0){
									// 登录成功后首次加载，跳转到第一个页面
									defaultPage = data.url;
									window.location.href = "${_path}/admin/admin#" + defaultPage;
									
								}
								else if(data.url == window.location.href.split("#")[1]){
									// 如果跳转页，和当前菜单对应
									defaultPage = data.url;
									html += 'active';
								}
							}
						
							html += '">';
							
							var childrens = _getChildrens(list, data.id);
							
							if(data.url){
								html += '	<a data-url="' + data.url + '" href="#' + data.url + '" class="' + (childrens.length > 0 ? 'dropdown-toggle' : '') + '">';
							}
							else{
								html += '	<a href="#" class="' + (childrens.length > 0 ? 'dropdown-toggle' : '') + '">';
							}
							html += '		<i class="menu-icon fa ' + data.icon + '"></i>';
							html += '		<span class="menu-text">' + data.name + ' </span>';
							html += '		<b class="arrow' + (childrens.length > 0 ? ' fa fa-angle-down' : '') + '"></b>';
							html += '	</a>';
							
							html += '	<b class="arrow"></b>';
							
							if (childrens.length > 0) {
								html += buildTree(list, childrens);
							}
							html += '</li>';
						}
					}
					return html;
				}
				
				function buildTree(list, childrens) {
					var html = "";
					if (childrens.length > 0) {
						html += '	<ul class="submenu">';
						for ( var i = 0; i < childrens.length; i++) {
							data = childrens[i];
							
							if(defaultPage == null && data.url){
								if(window.location.href.indexOf("#") < 0){
									// 登录成功后首次加载，跳转到第一个页面
									defaultPage = data.url;
									window.location.href = "${_path}/admin/admin#" + defaultPage;
								}
							}
							
							html += '<li class="">';
							
							var tempChildrens = _getChildrens(list, data.id);
							if(data.url){
								html += '	<a data-url="' + data.url + '" href="#' + data.url + '" class="' + (tempChildrens.length > 0 ? 'dropdown-toggle' : '') + '">';
							}
							else{
								html += '	<a href="#" class="' + (tempChildrens.length > 0 ? 'dropdown-toggle' : '') + '">';
							}
							html += '		<i class="menu-icon fa ' + data.icon + '"></i>';
							html += '		<span class="menu-text">' + data.name + ' </span>';
							html += '		<b class="arrow' + (tempChildrens.length > 0 ? ' fa fa-angle-down' : '') + '"></b>';
							html += '	</a>';
							
							html += '	<b class="arrow"></b>';
							
							
							if (tempChildrens.length > 0) {
								html += buildTree(list, tempChildrens, data.id);
							}
							html += '</li>';
						}
						html += '	</ul>';
					}
					return html;
				}
				
				function _getChildrens(list, id) {
					var children = new Array();
					var child = null;
					for ( var i = 0; i < list.length; i++) {
						child = new Object();
						if (list[i].parentId == id) {
							child.id = list[i].id;
							child.parentId = list[i].parentId;
							child.name = list[i].name;
							child.url = list[i].url;
							child.icon = list[i].icon;
							children.push(child);
						}
					}
					return children;
				}
			});
		</script>
	</body>
</html>
