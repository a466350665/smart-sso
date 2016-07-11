package com.smart.sso.client;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.ssm.config.ConfigUtils;
import com.smart.ssm.util.SpringUtils;

/**
 * 权限变更消息监听
 * 
 * @author Joe
 */
public class PermissionJmsListener implements MessageListener {
	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionJmsListener.class);

	@Override
	public void onMessage(Message message) {
		String appCode = null;
		try {
			appCode = ((TextMessage) message).getText();
		}
		catch (JMSException e) {
			LOGGER.error("Jms illegal message!");
		}

		if (ConfigUtils.getProperty("app.code").equals(appCode)) {
			SsoRealm realm = SpringUtils.getBean(SsoRealm.class);
			realm.clearAllCachedAuthorizationInfo();
			realm.refreshApplicationPermissions();
			LOGGER.info("appCode为：{}的应用权限更新成功！", appCode);
		}
	}
}
