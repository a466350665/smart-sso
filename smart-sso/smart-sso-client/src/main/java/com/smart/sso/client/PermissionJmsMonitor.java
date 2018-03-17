package com.smart.sso.client;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * 权限变动监控
 * 
 * @author Joe
 */
public class PermissionJmsMonitor {

	/**
	 * MQ通知权限变动，现有登录用户重新获取权限
	 */
	public static volatile boolean isChanged = false;

	/**
	 * 当权限发生变动，也就是说isChanged = true时，存储已获取最新权限的token集合
	 */
	public static Set<String> tokenSet = new CopyOnWriteArraySet<String>();
}
