package com.smart.sso.client;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.smart.sso.rpc.AuthenticationRpcService;

/**
 * 权限变更消息监听
 * 
 * @author Joe
 */
public class PermissionJmsListener implements MessageListener {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionJmsListener.class);

	@Value("${sso.app.code}")
	private String ssoAppCode;
	
	@Autowired
	private AuthenticationRpcService authenticationRpcService;
	
	@Override
	public void onMessage(Message message) {
		String appCode = null;
		try {
			appCode = ((TextMessage) message).getText();
		}
		catch (JMSException e) {
			LOGGER.error("Jms illegal message!");
		}

		if (ssoAppCode.equals(appCode)) {
			// 1.通知当前子系统权限有变动修改
			PermissionJmsMonitor.isChanged = true;
			// 2.清除已获取最新权限的token集合(Session级别)
			PermissionJmsMonitor.tokenSet.clear();
			// 3.更新应用权限（Application级别）
			ApplicationPermissionUtils.initApplicationPermissions(authenticationRpcService, ssoAppCode);
			LOGGER.info("成功通知appCode为：{}的应用更新权限！", appCode);
		}
	}
}
