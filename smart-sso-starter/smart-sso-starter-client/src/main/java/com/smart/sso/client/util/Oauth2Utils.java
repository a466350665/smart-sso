package com.smart.sso.client.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.smart.sso.base.constant.Oauth2Constant;
import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.Result;
import com.smart.sso.base.enums.GrantTypeEnum;
import com.smart.sso.base.util.HttpUtils;
import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.client.ClientProperties;
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
     * 发送http请求获取accessToken
     *
     * @param properties
     * @param code
     */
    public static AccessToken getHttpAccessToken(ClientProperties properties, String code) {
        Result<AccessToken> result = getAccessToken(properties.getServerUrl(), properties.getAppId(),
                properties.getAppSecret(), code);
        if (!result.isSuccess()) {
            logger.error("getHttpAccessToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
    }

    /**
     * 发送http请求刷新token
     *
     * @param properties
     * @param refreshToken
     * @return
     */
    public static AccessToken getHttpRefreshToken(ClientProperties properties, String refreshToken) {
        Result<AccessToken> result = getRefreshToken(properties.getServerUrl(), properties.getAppId(), refreshToken);
        if (!result.isSuccess()) {
            logger.error("getHttpRefreshToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
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
    private static Result<AccessToken> getAccessToken(String serverUrl, String appId, String appSecret, String code) {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put(Oauth2Constant.GRANT_TYPE, GrantTypeEnum.AUTHORIZATION_CODE.getValue());
        paramMap.put(Oauth2Constant.APP_ID, appId);
        paramMap.put(Oauth2Constant.APP_SECRET, appSecret);
        paramMap.put(Oauth2Constant.AUTH_CODE, code);
        return getHttpToken(serverUrl + Oauth2Constant.ACCESS_TOKEN_URL, paramMap);
    }

    /**
     * 刷新accessToken
     *
     * @param serverUrl
     * @param appId
     * @param refreshToken
     * @return
     */
    private static Result<AccessToken> getRefreshToken(String serverUrl, String appId, String refreshToken) {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put(Oauth2Constant.APP_ID, appId);
        paramMap.put(Oauth2Constant.REFRESH_TOKEN, refreshToken);
        return getHttpToken(serverUrl + Oauth2Constant.REFRESH_TOKEN_URL, paramMap);
    }

    private static Result<AccessToken> getHttpToken(String url, Map<String, String> paramMap) {
        String jsonStr = HttpUtils.get(url, paramMap);
        if (jsonStr == null || jsonStr.isEmpty()) {
            logger.error("get http token return null. url:{}", url);
            return Result.createError("获取token失败");
        }
        Result<AccessToken> result = JsonUtils.parseObject(jsonStr, new TypeReference<Result<AccessToken>>() {
        });
        if (result == null) {
            logger.error("parse accessToken return null. jsonStr:{}", jsonStr);
            return Result.createError("解析token失败");
        }
        return result;
    }
}