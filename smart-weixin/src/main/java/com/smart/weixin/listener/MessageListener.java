package com.smart.weixin.listener;

import java.util.Map;

import com.smart.weixin.message.response.BaseMessage;

/**
 * 消息监听接口
 */
public interface MessageListener<T extends BaseMessage> {

	/**
	 * @param map request的xml参数解析后的Map集合
	 * @return 消息对象
	 * 注: 如果不需要返回消息到微信客户端，可返回new BaseMessage();
	 */
	public T getMessage(Map<String, String> map);

}
