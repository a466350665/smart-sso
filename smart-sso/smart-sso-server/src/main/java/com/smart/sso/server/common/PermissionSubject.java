package com.smart.sso.server.common;

import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.mvc.config.ConfigUtils;
import com.smart.mvc.util.SpringUtils;
import com.smart.sso.server.service.PermissionJmsService;

/**
 * 权限主题抽象
 * 
 * 注： 1.保存所有注册到smart-sso-server的应用编码 2.当权限变动时，负责调用JMS消息通知应用更新权限
 * 
 * @author Joe
 */
public abstract class PermissionSubject {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionSubject.class);

	/**
	 * 注册观察者对象
	 * 
	 * @param observer
	 *            观察者对象
	 */
	public abstract void attach(String appCode);

	protected abstract Set<String> getAppCodesets();

	/**
	 * 通知所有注册应用更新权限
	 */
	public void update() {
		PermissionJmsService permissionJmsService = SpringUtils.getBean(PermissionJmsService.class);
		Set<String> sets = getAppCodesets();
		for (String appCode : sets) {
			try {
				permissionJmsService.send(ConfigUtils.getProperty("mq.permission.queue.prefix").concat(appCode),
						appCode);
				LOGGER.info("消息服务通知appCode为：{}的应用更新权限", appCode);
			}
			catch (Exception e) {
				LOGGER.error("消息服务通知appCode为：{}的应用更新权限异常", appCode, e);
			}
		}
	}
}
