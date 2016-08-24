package com.smart.sso.server.common;

import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.mvc.util.SpringUtils;

/**
 * 存储VT_USER信息，并提供操作方法
 * 
 * @author Joe
 */
public class TokenManager {

	private static Logger LOGGER = LoggerFactory.getLogger(TokenManager.class);

	private static final Timer timer = new Timer(true);
	private static final Config config = SpringUtils.getBean(Config.class);

	static {
		timer.schedule(new TimerTask() {

			@Override
			public void run() {
				for (Entry<String, DummyUser> entry : DATA_MAP.entrySet()) {
					String token = entry.getKey();
					DummyUser dummyUser = entry.getValue();
					// 当前时间大于过期时间
					if (new Date().compareTo(dummyUser.expired) > 0) {
						// 已过期，清除对应token
						DATA_MAP.remove(token);
						LOGGER.debug("token : " + token + "已失效");
					}
				}
			}
		}, 60 * 1000, 60 * 1000);
	}

	// 避免静态类被实例化
	private TokenManager() {
	}

	// 复合结构体，含loginUser与过期时间expried两个成员
	private static class DummyUser {
		private LoginUser loginUser;
		private Date expired; // 过期时间
	}

	// 令牌存储结构
	private static final Map<String, DummyUser> DATA_MAP = new ConcurrentHashMap<String, TokenManager.DummyUser>();

	/**
	 * 验证令牌有效性,有效则延长session生命周期
	 * 
	 * @param token
	 * @return
	 */
	public static LoginUser validate(String token) {
		DummyUser dummyUser = DATA_MAP.get(token);
		if (dummyUser != null) {
			dummyUser.expired = new Date(new Date().getTime() + config.getTokenTimeout() * 60 * 1000);
		}
		return dummyUser == null ? null : dummyUser.loginUser;
	}

	/**
	 * 用户授权成功后将授权信息存入
	 * 
	 * @param token
	 * @param loginUser
	 */
	public static void addToken(String token, LoginUser loginUser) {
		DummyUser dummyUser = new DummyUser();
		dummyUser.loginUser = loginUser;
		dummyUser.expired = new Date(new Date().getTime() + config.getTokenTimeout() * 60 * 1000);
		DATA_MAP.put(token, dummyUser);
	}

	public static void remove(String token) {
		if (token != null) {
			DATA_MAP.remove(token);
		}
	}
}
