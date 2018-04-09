package com.smart.sso.server.common;

import java.util.Timer;
import java.util.TimerTask;

/**
 * 令牌管理抽象
 * 
 * @author Joe
 */
public abstract class TokenManager {
	
	public static final String TOKEN = "token";

	// 令牌有效期，单位为秒，默认30分钟
	protected int tokenTimeout = 1800;

	private final Timer timer = new Timer(true);

	// 每分钟执行一次
	public TokenManager() {
		timer.schedule(new TimerTask() {
			@Override
			public void run() {
				verifyExpired();
			}
		}, 60 * 1000, 60 * 1000);
	}

	public void setTokenTimeout(int tokenTimeout) {
		this.tokenTimeout = tokenTimeout;
	}

	/**
	 * 验证失效token
	 */
	public abstract void verifyExpired();

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
