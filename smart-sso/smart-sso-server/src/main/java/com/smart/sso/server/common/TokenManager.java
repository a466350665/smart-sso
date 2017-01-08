package com.smart.sso.server.common;

import java.util.Date;
import java.util.Map.Entry;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 存储tokenUser信息，并提供操作方法
 * 
 * @author Joe
 */
public class TokenManager {

	private static Logger LOGGER = LoggerFactory.getLogger(TokenManager.class);

	private final Timer timer = new Timer(true);

	// 令牌有效期，单位为秒，默认30分钟
	private int tokenTimeout = 1800;
	// 令牌存储结构
	private final ConcurrentHashMap<String, DummyUser> tokenMap = new ConcurrentHashMap<String, TokenManager.DummyUser>();
	// 令牌存储结构并发监听对象
	private final Object tokenMapMonitor = new Object();

	// 避免静态类被实例化
	public TokenManager() {
		timer.schedule(new TimerTask() {
			@Override
			public void run() {
				Date now = new Date();
				for (Entry<String, DummyUser> entry : tokenMap.entrySet()) {
					String token = entry.getKey();
					DummyUser dummyUser = entry.getValue();
					// 当前时间大于过期时间
					if (now.compareTo(dummyUser.expired) > 0) {
						synchronized (tokenMapMonitor) {
							// 已过期，清除对应token
							if (now.compareTo(dummyUser.expired) > 0) {
								tokenMap.remove(token);
								LOGGER.debug("token : " + token + "已失效");
							}
						}
					}
				}
			}
		}, 60 * 1000, 60 * 1000);
	}

	/**
	 * 验证令牌有效性,有效则延长session生命周期
	 * 
	 * @param token
	 * @return
	 */
	public LoginUser validate(String token) {
		DummyUser dummyUser = tokenMap.get(token);
		if (dummyUser != null) {
			extendeExpiredTime(dummyUser);
		}
		return dummyUser == null ? null : dummyUser.loginUser;
	}

	/**
	 * 用户授权成功后将授权信息存入
	 * 
	 * @param token
	 * @param loginUser
	 */
	public void addToken(String token, LoginUser loginUser) {
		DummyUser dummyUser = new DummyUser();
		dummyUser.loginUser = loginUser;
		extendeExpiredTime(dummyUser);
		tokenMap.putIfAbsent(token, dummyUser);
	}
	
	public void remove(String token) {
		tokenMap.remove(token);
	}
	
	public String existsLoginUser(LoginUser loginUser){
		synchronized (tokenMapMonitor) {
    		for (Entry<String, DummyUser> entry : tokenMap.entrySet()) {
    			String token = entry.getKey();
    			DummyUser dummyUser = entry.getValue();
    			// 当前时间大于过期时间
    			if (dummyUser.loginUser.equals(loginUser)) {
    				extendeExpiredTime(dummyUser);
    				return token;
    			}
    		}
		}
		return null;
	}
	
	/**
	 * 扩展过期时间
	 * @param dummyUser
	 */
	private void extendeExpiredTime(DummyUser dummyUser){
		dummyUser.expired = new Date(new Date().getTime() + tokenTimeout * 1000);
	}

	// 复合结构体，含loginUser与过期时间expried两个成员
	private class DummyUser {
		private LoginUser loginUser;
		private Date expired; // 过期时间
		
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			DummyUser other = (DummyUser) obj;
			if (loginUser == null) {
				if (other.loginUser != null)
					return false;
			}
			else if (!loginUser.equals(other.loginUser))
				return false;
			return true;
		}
	}

	public void setTokenTimeout(int tokenTimeout) {
		this.tokenTimeout = tokenTimeout;
	}
}
