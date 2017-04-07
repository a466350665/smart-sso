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
		"${_staticPath}/custom/jquery.form.min.js?v=" + Math.random(),
		// 列表
		"${_staticPath}/custom/jquery.table.min.js?v=" + Math.random(),
		// 确认框
		"${_staticPath}/assets/js/bootbox.js?v=" + Math.random(),
		"${_staticPath}/custom/assets/bootbox.custom.js?v=" + Math.random(),
		// 自动隐藏的提醒框
		"${_staticPath}/assets/js/jquery.gritter.js?v=" + Math.random(),
		"${_staticPath}/custom/assets/jquery.gritter.custom.js?v=" + Math.random(),
		// UI
		"${_staticPath}/assets/js/jquery-ui.custom.js?v=" + Math.random(),
		// 验证
		"${_staticPath}/custom/jquery.validate-2.0.min.js?v=" + Math.random(),
		"${_staticPath}/custom/jquery.validate-2.0.custom.min.js?v=" + Math.random()
	];
</script>
