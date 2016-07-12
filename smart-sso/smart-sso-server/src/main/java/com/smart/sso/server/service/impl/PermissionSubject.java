package com.smart.sso.server.service.impl;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.annotation.Resource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.smart.ssm.config.ConfigUtils;
import com.smart.sso.server.service.PermissionJmsService;

/**
 * 权限主题
 * 
 * 注：
 * 1.保存所有注册到smart-sso-server的应用
 * 2.当权限变动时，负责调用JMS消息通知应用更新权限
 * 
 * @author Joe
 */
@Component
public class PermissionSubject {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionSubject.class);
	
	/**
	 * 保存所有注册到smart-sso-server的应用编码,支持并发读写
	 */
	protected Set<String> sets = new CopyOnWriteArraySet<String>();
	
	@Resource
	private PermissionJmsService permissionJmsService;

	/**
	 * 注册观察者对象
	 * 
	 * @param observer
	 *            观察者对象
	 */
	public void attach(String appCode) {
		sets.add(appCode);
	}

	/**
	 * 通知appCode应用更新权限
	 * 
	 * @param appCode
	 *            应用编码
	 */
	public void update(String appCode) {
		for (String code : sets) {
			if (code.equals(appCode)) {
				try {
    				permissionJmsService.send(ConfigUtils.getProperty("mq.permission.queue.prefix").concat(appCode), appCode);
    				LOGGER.info("消息服务通知appCode为：{}的应用更新权限", appCode);
				}
				catch (Exception e) {
					LOGGER.error("消息服务通知appCode为：{}的应用更新权限异常", appCode, e);
				}
			}
		}
	}
	
	/**
	 * 通知所有注册应用更新权限
	 */
	public void update() {
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
