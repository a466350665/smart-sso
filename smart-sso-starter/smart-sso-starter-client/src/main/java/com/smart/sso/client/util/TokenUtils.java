package com.smart.sso.client.util;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.Userinfo;
import com.smart.sso.base.util.CookieUtils;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.constant.ClientConstant;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.TokenWrapper;
import org.springframework.util.StringUtils;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Optional;

/**
 * Token工具
 *
 * @author Joe
 */
public class TokenUtils {

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
            AccessToken at = Oauth2Utils.getHttpRefreshToken(properties, wrapper.getObject().getRefreshToken());
            if (at != null) {
                tokenStorage.remove(token);
                tokenStorage.create(at.getAccessToken(), new TokenWrapper(at, at.getExpiresIn(), at.getRefreshExpiresIn()));

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
        String token = getCookieToken(request);
        // cookie中没有
        if (StringUtils.isEmpty(token)) {
            token = at.getAccessToken();
            // 写入cookie
            addCookieToken(token, request, response);
        }
        tokenStorage.create(token, new TokenWrapper(at, at.getExpiresIn(), at.getRefreshExpiresIn()));
    }

    private static void addCookieToken(String token, HttpServletRequest request, HttpServletResponse response) {
        CookieUtils.addCookie(ClientConstant.COOKIE_TOKEN, token, "/", request, response);
    }

    private static String getCookieToken(HttpServletRequest request) {
        return CookieUtils.getCookieValue(ClientConstant.COOKIE_TOKEN, request);
    }
}