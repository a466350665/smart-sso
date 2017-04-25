package com.smart.sso.server.common;

/**
 * 存储tokenUser信息，并提供操作方法
 * 
 * @author Joe
 */
public abstract class TokenManager {

	// 令牌有效期，单位为秒，默认30分钟
	protected int tokenTimeout = 1800;

	public void setTokenTimeout(int tokenTimeout) {
		this.tokenTimeout = tokenTimeout;
	}

	/**
	 * 用户授权成功后将授权信息存入
	 * 
	 * @param token
	 * @param loginUser
	 */
	public abstract void addToken(String token, LoginUser loginUser);

	/**
	 * 验证令牌有效性,有效则延长session生命周期
	 * 
	 * @param token
	 * @return
	 */
	public abstract LoginUser validate(String token);

	/**
	 * 移除令牌
	 * 
	 * @param token
	 */
	public abstract void remove(String token);
}
