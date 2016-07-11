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
 * @author Joe
 */
@Component
public class PermissionSubject {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionSubject.class);
	
	/**
	 * 保存注册的应用编码,支持并发读写
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
	 * 权限更新
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
}
