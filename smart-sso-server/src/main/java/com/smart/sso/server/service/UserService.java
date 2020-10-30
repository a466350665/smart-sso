package com.smart.sso.server.service;

import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcUser;

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
	public Result<RpcUser> login(String account, String password);
}
