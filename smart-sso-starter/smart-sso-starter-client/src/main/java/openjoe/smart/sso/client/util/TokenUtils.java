package openjoe.smart.sso.client.util;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.entity.Userinfo;
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
                // 创建存储新token
                tokenStorage.create(token);

                // 更新Cookie中的token值
                CookieUtils.updateCookie(properties.getCookieName(), token.getAccessToken(), request);
                // 将新的token值重新写回客户端cookie
                addCookieAccessToken(token.getAccessToken(), request, response);
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

    public static Userinfo getUserinfo(HttpServletRequest request) {
        return Optional.ofNullable(get(request)).map(wrapper -> wrapper.getObject().getUserinfo()).orElse(null);
    }

    public static Object getAttribute(String attribute, HttpServletRequest request) {
        return Optional.ofNullable(get(request)).map(wrapper -> wrapper.getAttributes().get(attribute)).orElse(null);
    }

    public static void setAttribute(String attribute, Object value, HttpServletRequest request) {
        TokenWrapper wrapper = get(request);
        if (wrapper != null) {
            wrapper.getAttributes().put(attribute, value);
        }
    }

    public static void set(Token token, HttpServletRequest request, HttpServletResponse response) {
        // 创建存储token
        tokenStorage.create(token);
        // 写入cookie
        addCookieAccessToken(token.getAccessToken(), request, response);
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
    public static Token getHttpAccessToken(String code) {
        Result<Token> result = Oauth2Utils.getAccessToken(properties.getServerUrl(), properties.getAppId(),
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
     * @param refreshToken
     * @return
     */
    public static Token getHttpRefreshToken(String refreshToken) {
        Result<Token> result = Oauth2Utils.getRefreshToken(properties.getServerUrl(), properties.getAppId(), refreshToken);
        if (!result.isSuccess()) {
            logger.error("getHttpRefreshToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
    }
}