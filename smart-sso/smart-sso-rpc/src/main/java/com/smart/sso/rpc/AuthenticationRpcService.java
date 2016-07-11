package com.smart.sso.rpc;

import java.util.List;


/**
 * 身份认证系统通信服务接口
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
	 * 根据登录的Token和应用编码获取授权信息(用户、权限)
	 * 
	 * @param token
	 *            授权码
	 * @param appCode
	 *            应用编码
	 * @return
	 */
	public RpcUser findAuthInfo(String token, String appCode);
	
	/**
	 * 获取当前应用所有权限(含菜单)
	 * 
	 * @param appCode
	 *            应用编码
	 * @return
	 */
	public List<Menu> findPermissionList(String token, String appCode);
	
	/**
	 * 更新当前登录用户密码
	 * 
	 * @param token
	 *            授权码
	 * @param newPassword
	 *            新密码
	 * @return
	 */
	public boolean updatePassword(String token, String newPassword);
}
