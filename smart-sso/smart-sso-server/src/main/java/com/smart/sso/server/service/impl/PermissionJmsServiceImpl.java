package com.smart.sso.server.service.impl;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.stereotype.Component;

import com.smart.mvc.config.ConfigUtils;
import com.smart.mvc.util.SpringUtils;
import com.smart.sso.server.service.PermissionJmsService;

/**
 * 异步发送权限修改JMS消息
 * 
 * @author Joe
 */
@Component
public class PermissionJmsServiceImpl implements PermissionJmsService {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	@Override
	public void send(final String appCode) {
		JmsTemplate jmsTemplate = getJmsTemplate();
		if (jmsTemplate != null) {
			sendJmsMessage(jmsTemplate, appCode);
		}
	}

	private JmsTemplate getJmsTemplate() {
		JmsTemplate jmsTemplate = null;
		try {
			jmsTemplate = SpringUtils.getBean(JmsTemplate.class);
		}
		catch (Exception e) {
			logger.warn("jmsTemplate注入失败");
		}
		return jmsTemplate;
	}

	private void sendJmsMessage(JmsTemplate jmsTemplate, final String appCode) {
		try {
			String destinationName = ConfigUtils.getProperty("mq.permission.queue.prefix").concat(appCode);
			jmsTemplate.send(destinationName, new MessageCreator() {
				@Override
				public Message createMessage(Session session) throws JMSException {
					return session.createTextMessage(appCode);
				}
			});
			logger.info("消息服务通知appCode为：{}的应用更新权限", appCode);
		}
		catch (Exception e) {
			logger.error("消息服务通知appCode为：{}的应用更新权限异常", appCode, e);
		}
	}
}