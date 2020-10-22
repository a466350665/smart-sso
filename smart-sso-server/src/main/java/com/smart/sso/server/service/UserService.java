package com.smart.sso.server.service;

import com.smart.sso.client.dto.Result;
import com.smart.sso.client.dto.SsoUser;

/**
 * 用户服务接口
 * 
 * @author Joe
 */
public interface UserService {
	
	/**
	 * 登录
	 * 
	 * @param account
	 *            登录名
	 * @param password
	 *            密码
	 * @return
	 */
	public Result<SsoUser> login(String account, String password);
}
