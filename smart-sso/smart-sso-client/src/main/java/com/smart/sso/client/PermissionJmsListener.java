package com.smart.sso.client;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.context.ContextLoader;

/**
 * 权限变更消息监听
 * @author Joe
 */
public class PermissionJmsListener implements MessageListener {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	private String ssoAppCode;
	
	@Override
	public void onMessage(Message message) {
		String appCode = null;
		try {
			appCode = ((TextMessage) message).getText();
		}
		catch (JMSException e) {
			logger.error("Jms illegal message!", e);
		}

		if (ssoAppCode.equals(appCode)) {
			// 1.失效所有session权限（session级别）
			PermissionFilter.invalidateSessionPermissions();
			// 2.更新应用权限（Application级别）
			SmartContainer container = ContextLoader.getCurrentWebApplicationContext().getBean(SmartContainer.class);
			ApplicationPermission.initApplicationPermissions(container.getAuthenticationRpcService(), ssoAppCode);
			logger.info("成功通知appCode为：{}的应用更新权限！", appCode);
		}
	}

	public void setSsoAppCode(String ssoAppCode) {
		this.ssoAppCode = ssoAppCode;
	}
}
