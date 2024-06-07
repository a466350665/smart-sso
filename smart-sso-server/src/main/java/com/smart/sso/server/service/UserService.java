package com.smart.sso.server.service;

import com.smart.sso.base.entity.Result;
import com.smart.sso.base.entity.Userinfo;

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
	Result<Userinfo> login(String username, String password);
}
