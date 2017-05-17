/**
 * jQuery Box 1.0
 */
var smart = smart ? smart : {};
(function(sm, $) {
	sm.box = {
		options : {
			// 类型["alert", "confirm"]
			type : "alert",
			// 是否显示遮罩层
			mask : true,
			// 定时关闭
			time : 0,
			
			
			// 消息标题
			title : '温馨提示',
			// 状态显示样式['ok', 'warn']
			status : null,
			// 消息内容
			content : '',
			// 宽
			width : 400,
			// 高
			height : 150,
			
			
			// 关闭之前
			beforeClose : function() {
			},
			// 关闭后
			afterClose : function() {
			},
			
			// 类型为"confirm"，确认按钮执行
			confirmButton : "确认",
			confirm : function() {
			},
			cancelButton : "取消",
			cancel : function() {
			}
		},
		object : null, // 存取box对象
		createMask : function () {
			return '<div id="_boxMask" class="box_mask" style="width: ' + $(document).width() + 'px; height: ' + $(document).height() + 'px"/>';
		},
		createTitle : function (o) {
			var html = '';
			html += '			<div class="box_title_bar">';
			html += '				<div class="box_title">' + o.title + '</div>';
			html += '				<a id="_boxClose" class="box_close">×</a>';
			html += '			</div>';
			return html;
		},
		bindEvent : function (o) {
			$('#_boxClose').click(function(){
				sm.box.close();
			});
			
			$("#_boxConfirm").click(function() {
				o.confirm();
				sm.box.close();
			});
			
			$("#_boxCancle").click(function() {
				o.cancel();
				sm.box.close();
			});
			
			if(o.time > 0){
				setTimeout(function(){
					sm.box.close();
				}, o.time);
			}
		},
		unbindEvent : function (o) {
			$("#_boxClose").unbind("click");
			$("#_boxConfirm").unbind("click");
			$("#_boxCancle").unbind("click");
		},
		createContent : function (o) {
			var html = '';
			html += '			<div class="box_content">';
			html += '				<table>';
			html += '					<tr>';
			html += '						<td>';
			html += '							<span id="_boxStatus" class="box_status' + (o.status != null ? ' box_status_' + o.status : '') + '"/>';
			html += '						</td>';
			if (o.content != null) {
				html += '						<td id="_boxMessage" class="box_message">' + o.content + '</td>';
			}
			html += '					</tr>';
			html += '				</table>';
			html += '			</div>';
			return html;
		},
		createButton : function (o) {
			var html = '';
			html += '		<div class="box_dialog_Button">';
			html += '			<a id="_boxConfirm">' + o.confirmButton + '</a>';
			html += '			<a id="_boxCancle">' + o.cancelButton + '</a>';
			html += '		</div>';
			return html;
		},
		create : function (o) {
			var html = '';
			if(o.mask){
				html += sm.box.createMask();
			}
			html += '<div id="_boxContainer" class="box_container">';
			html += '	<div class="box_dialog" style="width: ' + o.width + 'px;">';
			if (o.content != null) {
				html += '		<div class="box_dialog_content"  style="width: ' + o.width + 'px; height: ' + o.height + 'px">';
				html += sm.box.createTitle(o);
				html += sm.box.createContent(o);
				html += '		</div>';
			}
			if(o.type == 'confirm'){
				html += sm.box.createButton(o);
			}
			html += '	</div>';
			html += '</div>';
			$('body').append(html);
		},
		destory : function () {
		},
		open : function(o) {
			o = $.extend({}, sm.box.options, o);
			//if(sm.box.object == null){
				sm.box.create(sm.box.object = o);
			//}
			sm.box.bindEvent(sm.box.object);
			if(o.mask){
				$("#_boxMask").show();
			}
			$("#_boxContainer").fadeIn(300);
		},
		close : function() {
			sm.box.object.beforeClose();
			sm.box.destory();
			sm.box.unbindEvent(sm.box.object);
			$("#_boxContainer").fadeOut(300);
			$("#_boxContainer").remove();
			if(sm.box.object.mask){
				$("#_boxMask").hide();
				$("#_boxMask").remove();
			}
			sm.box.object.afterClose();
		}
	};
})(smart, jQuery);

jQuery(function($) {
	$.box = function(c) {
		return smart.box.open(c);
	};

	$.boxClose = function(c) {
		return smart.box.close(c);
	};
});