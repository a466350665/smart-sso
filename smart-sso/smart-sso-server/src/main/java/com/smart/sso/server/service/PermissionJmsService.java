package com.smart.sso.server.service;

/**
 * 权限修改消息服务接口
 * 
 * @author Joe
 *
 */
public interface PermissionJmsService {

	/**
	 * 发送队列消息对象
	 * 
	 * @param destinationName 服务目的地名称
	 * @param appCode 应用编码
	 */
	public void send(String destinationName, String appCode);
}