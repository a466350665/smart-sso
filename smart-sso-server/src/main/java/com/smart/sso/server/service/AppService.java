package com.smart.sso.server.service;

import com.smart.sso.base.entity.Result;

/**
 * 应用服务接口
 * 
 * @author Joe
 */
public interface AppService {

	boolean exists(String appId);
	
	Result<Void> validate(String appId, String appSecret);
}
