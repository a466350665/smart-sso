package com.smart.sso.rpc;

import java.util.List;


/**
 * 身份认证授权服务接口
 * 
 * @author Joe
 */
public interface AuthenticationRpcService {
	
	/**
	 * 验证是否已经登录
	 * 
	 * @param token
	 *            授权码
	 * @return
	 */
	public boolean validate(String token);

	/**
	 * 根据登录的Token和应用编码获取授权用户信息
	 * 
	 * @param token
	 *            授权码
	 * @param appCode
	 *            应用编码
	 * @return
	 */
	public RpcUser findAuthInfo(String token);
	
	/**
	 * 获取当前应用所有权限(含菜单)
	 * 
	 * @param token
	 *            授权码 (如果token不为空，获取当前用户的所有权限)
	 * @param appCode
	 *            应用编码
	 * @return
	 */
	public List<RpcPermission> findPermissionList(String token, String appCode);
}
