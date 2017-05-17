package com.smart.sso.server.common;

import java.util.Set;

import javax.annotation.Resource;

import com.smart.mvc.cache.RedisCache;

/**
 * 分布式环境权限主题
 * 
 * @author Joe
 */
public class RedisPermissionSubject extends PermissionSubject {

	public static final String PERMISSION_SUBJECT = "permissionSubject";

	@Resource
	private RedisCache<String> redisCache;

	public void attach(String appCode) {
		redisCache.setSet(PERMISSION_SUBJECT, appCode);
	}

	@Override
	protected Set<String> getAppCodeSet() {
		return redisCache.getSet(PERMISSION_SUBJECT);
	}

}
