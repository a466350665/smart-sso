package com.smart.sso.client.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.util.HttpUtils;
import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.client.constant.Oauth2Constant;
import com.smart.sso.client.entity.Result;
import com.smart.sso.client.enums.GrantTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

/**
 * Oauth2辅助类
 * 
 * @author Joe
 */
public class Oauth2Utils {

	private static final Logger logger = LoggerFactory.getLogger(Oauth2Utils.class);

	/**
	 * 获取accessToken（密码模式，app通过此方式由客户端代理转发http请求到服务端获取accessToken）
	 * 
	 * @param serverUrl
	 * @param appId
	 * @param appSecret
	 * @param username
	 * @param password
	 * @return
	 */
	public static Result<AccessToken> getAccessToken(String serverUrl, String appId, String appSecret, String username,
													 String password) {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put(Oauth2Constant.GRANT_TYPE, GrantTypeEnum.PASSWORD.getValue());
		paramMap.put(Oauth2Constant.APP_ID, appId);
		paramMap.put(Oauth2Constant.APP_SECRET, appSecret);
		paramMap.put(Oauth2Constant.USERNAME, username);
		paramMap.put(Oauth2Constant.PASSWORD, password);
		return getHttpAccessToken(serverUrl + Oauth2Constant.ACCESS_TOKEN_URL, paramMap);
	}

	/**
	 * 获取accessToken（授权码模式）
	 * 
	 * @param serverUrl
	 * @param appId
	 * @param appSecret
	 * @param code
	 * @return
	 */
	public static Result<AccessToken> getAccessToken(String serverUrl, String appId, String appSecret, String code) {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put(Oauth2Constant.GRANT_TYPE, GrantTypeEnum.AUTHORIZATION_CODE.getValue());
		paramMap.put(Oauth2Constant.APP_ID, appId);
		paramMap.put(Oauth2Constant.APP_SECRET, appSecret);
		paramMap.put(Oauth2Constant.AUTH_CODE, code);
		return getHttpAccessToken(serverUrl + Oauth2Constant.ACCESS_TOKEN_URL, paramMap);
	}

	/**
	 * 刷新accessToken
	 * 
	 * @param serverUrl
	 * @param appId
	 * @param refreshToken
	 * @return
	 */
	public static Result<AccessToken> refreshToken(String serverUrl, String appId, String refreshToken) {
		Map<String, String> paramMap = new HashMap<>();
		paramMap.put(Oauth2Constant.APP_ID, appId);
		paramMap.put(Oauth2Constant.REFRESH_TOKEN, refreshToken);
		return getHttpAccessToken(serverUrl + Oauth2Constant.REFRESH_TOKEN_URL, paramMap);
	}

	private static Result<AccessToken> getHttpAccessToken(String url, Map<String, String> paramMap) {
		String jsonStr = HttpUtils.get(url, paramMap);
		if (jsonStr == null || jsonStr.isEmpty()) {
			logger.error("getHttpAccessToken exception, return null. url:{}", url);
			return null;
		}
		return JsonUtils.parseObject(jsonStr, new TypeReference<Result<AccessToken>>(){});
	}
}