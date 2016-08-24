package com.smart.weixin.listener;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import com.smart.mvc.util.SpringUtils;
import com.smart.weixin.message.response.BaseMessage;
import com.smart.weixin.util.MessageUtils;

/**
 * 核心监听及任务分派类
 */
public class CoreListener {

	/**
	 * 处理微信发来的请求
	 * @throws Exception 
	 */
	public static String process(HttpServletRequest request) throws Exception {
		String responseMessage = null;
		// xml请求解析
		Map<String, String> requestMap = MessageUtils.parseXml(request);
		
		// 保存项目根路径
		requestMap.put("basePath", getBasePath(request));
		
		// 消息监听
		BaseMessage message = null;
		if ((message = getValidateMessage(requestMap)) != null
				|| (message = getTypeMessage(requestMap)) != null
				|| (message = getDefaultMessage(requestMap)) != null) {
			responseMessage = MessageUtils.messageToXml(message);
		}
		return responseMessage;
	}
	
	private static String getBasePath(HttpServletRequest request) {
		StringBuilder basePath = new StringBuilder(request.getScheme()).append("://").append(request.getServerName());
		if (80 != request.getServerPort()) {
			basePath.append(":").append(request.getServerPort());
		}
		return basePath.append(request.getContextPath()).append("/").toString();
	}

	private static BaseMessage getMessage(Map<String, String> map,
			String listener) {
		MessageListener<?> messageListener = (MessageListener<?>) SpringUtils
				.getBean(listener);
		return messageListener == null ? null : messageListener.getMessage(map);
	}

	/**
	 * 获取验证消息
	 */
	private static BaseMessage getValidateMessage(Map<String, String> map) {
		return getMessage(map, "validateMessageListener");
	}

	/**
	 * 获取正确的某种消息
	 */
	private static BaseMessage getTypeMessage(Map<String, String> map) {
		MessageListener<?> messageListener = null;
		String msgType = map.get("MsgType");
		// 文本消息
		if (msgType.equals(MessageUtils.REQUEST_MESSAGE_TYPE_TEXT)) {
			messageListener = (MessageListener<?>) SpringUtils
					.getBean("textMessageListener");
		}
		// 图片消息
		else if (msgType.equals(MessageUtils.REQUEST_MESSAGE_TYPE_IMAGE)) {
			messageListener = (MessageListener<?>) SpringUtils
					.getBean("locationMessageListener");
		}
		// 地理位置消息
		else if (msgType.equals(MessageUtils.REQUEST_MESSAGE_TYPE_LOCATION)) {
			messageListener = (MessageListener<?>) SpringUtils
					.getBean("locationMessageListener");
		}
		// 事件推送
		else if (msgType.equals(MessageUtils.REQUEST_MESSAGE_TYPE_EVENT)) {
			// 事件类型
			String eventType = map.get("Event");
			// 订阅
			if (eventType.equals(MessageUtils.EVENT_TYPE_SUBSCRIBE)) {
				messageListener = (MessageListener<?>) SpringUtils
						.getBean("subcribeMessageListener");
			}
			// 取消订阅
			else if (eventType.equals(MessageUtils.EVENT_TYPE_UNSUBSCRIBE)) {
				messageListener = (MessageListener<?>) SpringUtils
						.getBean("cancleSubcribeMessageListener");
			}
			// 自定义菜单点击事件
			else if (eventType.equals(MessageUtils.EVENT_TYPE_CLICK)) {
				messageListener = (MessageListener<?>) SpringUtils
						.getBean("menuMessageListener");
			}
			// 获取地理位置事件
			else if (eventType.equals(MessageUtils.EVENT_TYPE_LOCATION)) {
				messageListener = (MessageListener<?>) SpringUtils
						.getBean("eventLocationMessageListener");
			}
		}
		return messageListener == null ? null : messageListener.getMessage(map);
	}

	/**
	 * 获取没有匹配结果，默认的消息
	 */
	private static BaseMessage getDefaultMessage(Map<String, String> map) {
		return getMessage(map, "defaultMessageListener");
	}
}
