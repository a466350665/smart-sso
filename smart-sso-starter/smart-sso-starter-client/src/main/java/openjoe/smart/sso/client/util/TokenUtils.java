package openjoe.smart.sso.client.util;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.base.util.CookieUtils;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.token.TokenStorage;
import openjoe.smart.sso.client.token.TokenWrapper;
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

    private static ClientProperties properties;
    private static TokenStorage tokenStorage;

    public static void setTokenStorage(ClientProperties cp, TokenStorage ts) {
        properties = cp;
        tokenStorage = ts;
    }

    /**
     * 获取Token
     * 1.如果获取accessToken没过期，直接返回
     * 2.如果获取accessToken已过期，refreshToken没过期，使用refresh接口刷新再返回
     *
     * @param request
     * @return
     */
    public static Token getAndRefresh(HttpServletRequest request, HttpServletResponse response) {
        String accessToken = getCookieAccessToken(request);
        // cookie中没有
        if (!StringUtils.hasLength(accessToken)) {
            return null;
        }
        TokenWrapper wrapper = tokenStorage.get(accessToken);
        if (wrapper == null) {
            return null;
        }
        // accessToken没过期直接返回
        if (!wrapper.checkExpired()) {
            return wrapper.getObject();
        }
        // accessToken已过期，refreshToken没过期，使用refresh接口刷新
        if (!wrapper.checkRefreshExpired()) {
            Token token = getHttpRefreshToken(wrapper.getObject().getRefreshToken());
            if (token != null) {
                // 删除旧token
                tokenStorage.remove(accessToken);
                // 更新Cookie中的token值
                CookieUtils.updateCookie(properties.getCookieName(), token.getAccessToken(), request);

                // 存储token
                set(token, request, response);
                return token;
            }
        }
        return null;
    }

    public static TokenWrapper get(HttpServletRequest request) {
        String accessToken = getCookieAccessToken(request);
        // cookie中没有
        if (!StringUtils.hasLength(accessToken)) {
            return null;
        }
        return tokenStorage.get(accessToken);
    }

    public static TokenUser getUser(HttpServletRequest request) {
        return Optional.ofNullable(get(request)).map(wrapper -> wrapper.getObject().getTokenUser()).orElse(null);
    }

    public static Long getUserId(HttpServletRequest request) {
        return Optional.ofNullable(getUser(request)).map(u -> u.getId()).orElse(null);
    }

    public static TokenPermission getPermission(HttpServletRequest request) {
        return Optional.ofNullable(get(request)).map(wrapper -> wrapper.getObject().getTokenPermission()).orElse(null);
    }

    public static void set(Token token, HttpServletRequest request, HttpServletResponse response) {
        set(token);
        // 写入cookie
        addCookieAccessToken(token.getAccessToken(), request, response);
    }

    public static void set(Token token) {
        // 创建存储token
        tokenStorage.create(token);
    }

    private static void addCookieAccessToken(String accessToken, HttpServletRequest request, HttpServletResponse response) {
        CookieUtils.addCookie(properties.getCookieName(), accessToken, "/", request, response);
    }

    private static String getCookieAccessToken(HttpServletRequest request) {
        return CookieUtils.getCookieValue(properties.getCookieName(), request);
    }

    /**
     * 发送http请求获取accessToken
     *
     * @param code
     */
    public static Token getHttpAccessToken(String code, HttpServletRequest request) {
        Result<Token> result = Oauth2Utils.getAccessToken(properties.getServerUrl(), properties.getClientId(),
                properties.getClientSecret(), code, getLocalUrl(request) + properties.getLogoutPath());
        if (!result.isSuccess()) {
            logger.error("getHttpAccessToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
    }

    /**
     * 获取当前应用访问路径
     *
     * @param request
     * @return
     */
    private static String getLocalUrl(HttpServletRequest request) {
        StringBuilder url = new StringBuilder();
        url.append(request.getScheme()).append("://").append(request.getServerName());
        if (request.getServerPort() != 80 && request.getServerPort() != 443) {
            url.append(":").append(request.getServerPort());
        }
        url.append(request.getContextPath());
        return url.toString();
    }

    /**
     * 发送http请求刷新token
     *
     * @param refreshToken
     * @return
     */
    public static Token getHttpRefreshToken(String refreshToken) {
        Result<Token> result = Oauth2Utils.getRefreshToken(properties.getServerUrl(), properties.getClientId(), refreshToken);
        if (!result.isSuccess()) {
            logger.error("getHttpRefreshToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
    }
}