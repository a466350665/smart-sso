package com.smart.sso.server.service.impl;

import javax.annotation.Resource;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.stereotype.Component;

import com.smart.sso.server.service.PermissionJmsService;

/**
 * 异步发送权限修改JMS消息
 * 
 * @author Joe
 */
@Component
public class PermissionJmsServiceImpl implements PermissionJmsService {

	@Resource
	private JmsTemplate jmsTemplate;

	@Override
	public void send(String destinationName, final String appCode) {
		jmsTemplate.send(destinationName, new MessageCreator() {
			@Override
			public Message createMessage(Session session) throws JMSException {
				return session.createTextMessage(appCode);
			}
		});
	}
}