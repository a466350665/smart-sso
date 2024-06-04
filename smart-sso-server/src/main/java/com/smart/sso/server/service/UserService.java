package com.smart.sso.server.service;

import com.smart.sso.client.rpc.Result;
import com.smart.sso.server.common.ServerUser;

/**
 * 用户服务接口
 * 
 * @author Joe
 */
public interface UserService {
	
	/**
	 * 登录
	 * 
	 * @param username
	 *            登录名
	 * @param password
	 *            密码
	 * @return
	 */
	Result<ServerUser> login(String username, String password);
}
