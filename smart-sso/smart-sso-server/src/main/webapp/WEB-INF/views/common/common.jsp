<%@ page language="java" pageEncoding="utf-8"%>

<title>${param.title}-${_systemName}</title>

<!-- 文本框 -->
<link rel="stylesheet" href="${_staticPath}/assets/css/jquery-ui.custom.css" />
<!-- 多选框 -->
<link rel="stylesheet" href="${_staticPath}/assets/css/bootstrap-multiselect.css" />
<!-- 颜色选择  -->
<link rel="stylesheet" href="${_staticPath}/assets/css/colorpicker.css" />
<!-- 时间  -->
<link rel="stylesheet" href="${_staticPath}/assets/css/bootstrap-timepicker.css" />
<!-- 日期、日期+时间、日期范围  -->
<link rel="stylesheet" href="${_staticPath}/assets/css/daterangepicker.css" />
<!-- 提示框  -->
<link rel="stylesheet" href="${_staticPath}/assets/css/jquery.gritter.css" />
<!-- 拖拽式单文件上传 -->
<link rel="stylesheet" href="${_staticPath}/assets/css/dropzone.css" />
<link rel="stylesheet" href="${_staticPath}/assets/css/uploadifive.css" />

<!--[if lte IE 8]>
	<script src="${_staticPath}/assets/js/excanvas.js"></script>
<![endif]-->
<script type="text/javascript">
	var scripts = [
		// Form提交Json转换
		"${_staticPath}/custom/jquery.form.min.js",
		// 列表
		"${_staticPath}/custom/jquery.table.min.js?v=" + Math.random(),
		// 时间
		"${_staticPath}/assets/js/date-time/bootstrap-timepicker.js",
		// 时间支持
		"${_staticPath}/assets/js/date-time/moment.js",
		// 时间支持
		"${_staticPath}/assets/js/date-time/i18n/moment.zh-CN.js",
		// 日期范围
		"${_staticPath}/assets/js/date-time/daterangepicker.js",
		// 确认框
		"${_staticPath}/assets/js/bootbox.js",
		"${_staticPath}/custom/assets/bootbox.custom.js",
		// 自动隐藏的提醒框
		"${_staticPath}/assets/js/jquery.gritter.js",
		"${_staticPath}/custom/assets/jquery.gritter.custom.js",
		// UI
		"${_staticPath}/assets/js/jquery-ui.custom.js",
		// 文件上传
		"${_staticPath}/assets/js/dropzone.js",
		"${_staticPath}/assets/js/jquery.uploadifive.min.js",
		"${_staticPath}/custom/ajaxfileupload.js",
		// 验证
		"${_staticPath}/custom/jquery.validate-2.0.min.js",
		"${_staticPath}/custom/jquery.validate-2.0.custom.min.js"
	];
</script>