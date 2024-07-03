package openjoe.smart.sso.client.util;

import com.fasterxml.jackson.core.type.TypeReference;
import openjoe.smart.sso.base.constant.Oauth2Constant;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.enums.GrantTypeEnum;
import openjoe.smart.sso.base.util.HttpUtils;
import openjoe.smart.sso.base.util.JsonUtils;
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
     * 获取accessToken（授权码模式）
     *
     * @param serverUrl
     * @param appId
     * @param appSecret
     * @param code
     * @return
     */
    public static Result<Token> getAccessToken(String serverUrl, String appId, String appSecret, String code) {
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
    public static Result<Token> getRefreshToken(String serverUrl, String appId, String refreshToken) {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put(Oauth2Constant.APP_ID, appId);
        paramMap.put(Oauth2Constant.REFRESH_TOKEN, refreshToken);
        return getHttpToken(serverUrl + Oauth2Constant.REFRESH_TOKEN_URL, paramMap);
    }

    public static Result<Token> getHttpToken(String url, Map<String, String> paramMap) {
        String jsonStr = HttpUtils.get(url, paramMap);
        if (jsonStr == null || jsonStr.isEmpty()) {
            logger.error("get http token return null. url:{}", url);
            return Result.createError("获取token失败");
        }
        Result<Token> result = JsonUtils.parseObject(jsonStr, new TypeReference<Result<Token>>() {
        });
        if (result == null) {
            logger.error("parse accessToken return null. jsonStr:{}", jsonStr);
            return Result.createError("解析token失败");
        }
        return result;
    }
}