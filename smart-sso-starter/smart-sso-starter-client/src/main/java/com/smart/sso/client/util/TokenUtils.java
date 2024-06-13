package com.smart.sso.client.util;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.Result;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.CookieUtils;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.constant.ClientConstant;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.TokenWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Optional;

/**
 * Token工具
 *
 * @author Joe
 */
public class TokenUtils {

    private static final Logger logger = LoggerFactory.getLogger(TokenUtils.class);

    private static TokenStorage tokenStorage;

    public static void setTokenStorage(TokenStorage ts) {
        tokenStorage = ts;
    }

    /**
     * 获取accessToken
     * 1.如果获取accessToken没过期，直接返回
     * 2.如果获取accessToken已过期，refreshToken没过期，使用refresh接口刷新再返回
     *
     * @param properties
     * @param request
     * @return
     */
    public static AccessToken getAndRefresh(ClientProperties properties, HttpServletRequest request, HttpServletResponse response) {
        String token = getCookieToken(request);
        // cookie中没有
        if (StringUtils.isEmpty(token)) {
            return null;
        }
        TokenWrapper wrapper = tokenStorage.get(token);
        if (wrapper == null) {
            return null;
        }
        // accessToken没过期直接返回
        if (!wrapper.checkExpired()) {
            return wrapper.getObject();
        }
        // accessToken已过期，refreshToken没过期，使用refresh接口刷新
        if (!wrapper.checkRefreshExpired()) {
            AccessToken at = getHttpRefreshToken(properties, wrapper.getObject().getRefreshToken());
            if (at != null) {
                // 删除旧token
                tokenStorage.remove(token);
                // 创建存储新token
                tokenStorage.create(at);

                // 更新Cookie中的token值
                CookieUtils.updateCookie(ClientConstant.COOKIE_TOKEN, at.getAccessToken(), request);
                // 将新的token值重新写回客户端cookie
                addCookieToken(at.getAccessToken(), request, response);
                return at;
            }
        }
        return null;
    }

    public static AccessToken get(HttpServletRequest request) {
        String token = getCookieToken(request);
        // cookie中没有
        if (StringUtils.isEmpty(token)) {
            return null;
        }
        TokenWrapper wrapper = tokenStorage.get(token);
        if (wrapper == null) {
            return null;
        }
        return wrapper.getObject();
    }

    public static Userinfo getUserinfo(HttpServletRequest request) {
        return Optional.ofNullable(get(request)).map(u -> u.getUserinfo()).orElse(null);
    }

    public static void set(AccessToken at, HttpServletRequest request, HttpServletResponse response) {
        // 创建存储token
        tokenStorage.create(at);
        // 写入cookie
        addCookieToken(at.getAccessToken(), request, response);
    }

    private static void addCookieToken(String token, HttpServletRequest request, HttpServletResponse response) {
        CookieUtils.addCookie(ClientConstant.COOKIE_TOKEN, token, "/", request, response);
    }

    private static String getCookieToken(HttpServletRequest request) {
        return CookieUtils.getCookieValue(ClientConstant.COOKIE_TOKEN, request);
    }

    /**
     * 发送http请求获取accessToken
     *
     * @param properties
     * @param code
     */
    public static AccessToken getHttpAccessToken(ClientProperties properties, String code) {
        Result<AccessToken> result = Oauth2Utils.getAccessToken(properties.getServerUrl(), properties.getAppId(),
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
        Result<AccessToken> result = Oauth2Utils.getRefreshToken(properties.getServerUrl(), properties.getAppId(), refreshToken);
        if (!result.isSuccess()) {
            logger.error("getHttpRefreshToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
    }
}