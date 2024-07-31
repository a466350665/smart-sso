package openjoe.smart.sso.client.util;

import com.fasterxml.jackson.core.type.TypeReference;
import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Token;
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
     * @param clientId
     * @param clientSecret
     * @param code
     * @return
     */
    public static Result<Token> getAccessToken(String serverUrl, String clientId, String clientSecret, String code, String logoutUri) {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put(BaseConstant.GRANT_TYPE, GrantTypeEnum.AUTHORIZATION_CODE.getValue());
        paramMap.put(BaseConstant.CLIENT_ID, clientId);
        paramMap.put(BaseConstant.CLIENT_SECRET, clientSecret);
        paramMap.put(BaseConstant.AUTH_CODE, code);
        paramMap.put(BaseConstant.LOGOUT_URI, logoutUri);
        return getHttpToken(serverUrl + BaseConstant.ACCESS_TOKEN_PATH, paramMap);
    }

    /**
     * 刷新accessToken
     *
     * @param serverUrl
     * @param clientId
     * @param refreshToken
     * @return
     */
    public static Result<Token> getRefreshToken(String serverUrl, String clientId, String refreshToken) {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put(BaseConstant.CLIENT_ID, clientId);
        paramMap.put(BaseConstant.REFRESH_TOKEN, refreshToken);
        return getHttpToken(serverUrl + BaseConstant.REFRESH_TOKEN_PATH, paramMap);
    }

    public static Result<Token> getHttpToken(String url, Map<String, String> paramMap) {
        String jsonStr = HttpUtils.get(url, paramMap);
        if (jsonStr == null || jsonStr.isEmpty()) {
            logger.error("get http token return null. url:{}", url);
            return Result.error("获取token失败");
        }
        Result<Token> result = JsonUtils.parseObject(jsonStr, new TypeReference<Result<Token>>() {
        });
        if (result == null) {
            logger.error("parse accessToken return null. jsonStr:{}", jsonStr);
            return Result.error("解析token失败");
        }
        return result;
    }
}