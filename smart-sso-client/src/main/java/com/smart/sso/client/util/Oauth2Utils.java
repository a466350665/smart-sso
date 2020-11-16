package com.smart.sso.client.util;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSONObject;
import com.smart.sso.client.constant.Oauth2Constant;
import com.smart.sso.client.enums.GrantTypeEnum;
import com.smart.sso.client.rpc.Result;
import com.smart.sso.client.rpc.RpcAccessToken;

/**
 * Oauth2辅助类
 * 
 * @author Joe
 */
public class Oauth2Utils {

	private static final Logger logger = LoggerFactory.getLogger(Oauth2Utils.class);

	/**
	 * 密码模式获取accessToken（app通过此方式获取accessToken）
	 * 
	 * @param serverUrl
	 * @param appId
	 * @param appSecret
	 * @param username
	 * @param password
	 * @return
	 */
	public static RpcAccessToken getAccessToken(String serverUrl, String appId, String appSecret, String username,
			String password) {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put(Oauth2Constant.GRANT_TYPE, GrantTypeEnum.PASSWORD.getValue());
		paramMap.put(Oauth2Constant.APP_ID, appId);
		paramMap.put(Oauth2Constant.APP_SECRET, appSecret);
		paramMap.put(Oauth2Constant.USERNAME, username);
		paramMap.put(Oauth2Constant.PASSWORD, password);
		return getHttpJson(serverUrl + Oauth2Constant.ACCESS_TOKEN_URL, paramMap, RpcAccessToken.class);
	}

	/**
	 * 授权码模式获取accessToken
	 * 
	 * @param serverUrl
	 * @param appId
	 * @param appSecret
	 * @param code
	 * @return
	 */
	public static RpcAccessToken getAccessToken(String serverUrl, String appId, String appSecret, String code) {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put(Oauth2Constant.GRANT_TYPE, GrantTypeEnum.AUTHORIZATION_CODE.getValue());
		paramMap.put(Oauth2Constant.APP_ID, appId);
		paramMap.put(Oauth2Constant.APP_SECRET, appSecret);
		paramMap.put(Oauth2Constant.AUTH_CODE, code);
		return getHttpJson(serverUrl + Oauth2Constant.ACCESS_TOKEN_URL, paramMap, RpcAccessToken.class);
	}

	/**
	 * 刷新accessToken
	 * 
	 * @param serverUrl
	 * @param appId
	 * @param refreshToken
	 * @return
	 */
	public static RpcAccessToken refreshToken(String serverUrl, String appId, String refreshToken) {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put(Oauth2Constant.APP_ID, appId);
		paramMap.put(Oauth2Constant.REFRESH_TOKEN, refreshToken);
		return getHttpJson(serverUrl + Oauth2Constant.REFRESH_TOKEN_URL, paramMap, RpcAccessToken.class);
	}

	private static <T> T getHttpJson(String url, Map<String, String> paramMap, Class<T> clazz) {
		String jsonStr = HttpUtils.get(url, paramMap);
		if (jsonStr == null || jsonStr.isEmpty()) {
			logger.error("getHttpJson exception, return null. url:{}", url);
			return null;
		}
		Result<?> result = JSONObject.parseObject(jsonStr, Result.class);
		if (!result.isSuccess()) {
			logger.error("getHttpJson has error, url:{}, message:{}", url, result.getMessage());
			return null;
		}
		return JSONObject.parseObject(result.getData().toString(), clazz);
	}
}