package com.smart.sso.server.service.impl;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.stereotype.Component;

import com.smart.mvc.util.SpringUtils;
import com.smart.sso.server.service.PermissionJmsService;

/**
 * 异步发送权限修改JMS消息
 * 
 * @author Joe
 */
@Component
public class PermissionJmsServiceImpl implements PermissionJmsService {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionJmsServiceImpl.class);

	@Override
	public void send(String destinationName, final String appCode) {
		JmsTemplate jmsTemplate = null;
		try {
			jmsTemplate = SpringUtils.getBean(JmsTemplate.class);
		}
		catch (Exception e) {
			LOGGER.warn("jmsTemplate注入失败");
		}

		if (jmsTemplate != null) {
			jmsTemplate.send(destinationName, new MessageCreator() {
				@Override
				public Message createMessage(Session session) throws JMSException {
					return session.createTextMessage(appCode);
				}
			});
		}
	}
}