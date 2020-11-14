package com.smart.sso.client.util;

import java.text.MessageFormat;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSONObject;
import com.smart.sso.client.constant.Oauth2Constant;
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
	 * 获取授权码（app通过此方式由客户端代理转发http请求到服务端获取授权码）
	 * 
	 * @param serverUrl
	 * @param paramMap
	 * @return
	 */
	public static String getCode(String serverUrl, Map<String, String> paramMap) {
		String authorizeUrl = MessageFormat.format(Oauth2Constant.AUTHORIZE_URL, serverUrl);
		String resultStr = HttpUtils.post(authorizeUrl, paramMap);
		if (resultStr == null) {
			return null;
		}
		Result<?> result = JSONObject.parseObject(resultStr, Result.class);
		if (!result.isSuccess()) {
			logger.error("getCode has error, appLoginUrl:{}, message:{}", authorizeUrl, result.getMessage());
			return null;
		}
		return result.getData().toString();
	}
	
	/**
	 * 获取accessToken（app通过此方式获取accessToken）
	 * 
	 * @param serverUrl
	 * @param appId
	 * @param appSecret
	 * @param paramMap 可根据不同场景自定义登录参数
	 * @return
	 */
	public static RpcAccessToken getAccessToken(String serverUrl, String appId, String appSecret,
			Map<String, String> paramMap) {
		String code = getCode(serverUrl, paramMap);
		if (code == null) {
			return null;
		}
		return getAccessToken(serverUrl, appId, appSecret, code);
	}

	/**
	 * 获取accessToken
	 * 
	 * @param serverUrl
	 * @param appId
	 * @param appSecret
	 * @param code
	 * @return
	 */
	public static RpcAccessToken getAccessToken(String serverUrl, String appId, String appSecret, String code) {
		String accessTokenUrl = MessageFormat.format(Oauth2Constant.ACCESS_TOKEN_URL, serverUrl, appId, appSecret,
				code);
		return getHttpJson(accessTokenUrl, RpcAccessToken.class);
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
		String refreshTokenUrl = MessageFormat.format(Oauth2Constant.REFRESH_TOKEN_URL, serverUrl, appId, refreshToken);
		return getHttpJson(refreshTokenUrl, RpcAccessToken.class);
	}

	private static <T> T getHttpJson(String url, Class<T> clazz) {
		String jsonStr = HttpUtils.get(url);
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