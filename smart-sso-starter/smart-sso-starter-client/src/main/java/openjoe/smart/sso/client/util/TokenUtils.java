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
     * @return
     */
    public static Token getAndRefresh() {
        String accessToken = getAccessToken();
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
                CookieUtils.updateCookie(properties.getTokenName(), token.getAccessToken(), ClientContextHolder.getRequest());

                // 存储token并写入cookie
                setInCookie(token);
                return token;
            }
        }
        return null;
    }

    public static TokenWrapper get() {
        String accessToken = getAccessToken();
        // cookie中没有
        if (!StringUtils.hasLength(accessToken)) {
            return null;
        }
        return tokenStorage.get(accessToken);
    }

    public static TokenUser getUser() {
        return Optional.ofNullable(get()).map(wrapper -> wrapper.getObject().getTokenUser()).orElse(null);
    }

    public static Long getUserId() {
        return Optional.ofNullable(getUser()).map(u -> u.getId()).orElse(null);
    }

    public static TokenPermission getPermission() {
        return Optional.ofNullable(get()).map(wrapper -> wrapper.getObject().getTokenPermission()).orElse(null);
    }

    public static void setInCookie(Token token) {
        set(token);
        // 写入cookie
        addCookieAccessToken(token.getAccessToken());
    }

    public static void set(Token token) {
        // 创建存储token
        tokenStorage.create(token);
    }

    private static void addCookieAccessToken(String accessToken) {
        CookieUtils.addCookie(properties.getTokenName(), accessToken, "/", ClientContextHolder.getRequest(), ClientContextHolder.getResponse());
    }

    private static String getAccessToken() {
        String accessToken = getCookieAccessToken();
        if(StringUtils.hasLength(accessToken)){
            return accessToken;
        }
        return getHeaderAccessToken();
    }

    private static String getCookieAccessToken() {
        return CookieUtils.getCookieValue(properties.getTokenName(), ClientContextHolder.getRequest());
    }

    private static String getHeaderAccessToken() {
        return ClientContextHolder.getRequest().getHeader(properties.getTokenName());
    }

    /**
     * 发送http请求获取accessToken
     *
     * @param code
     */
    public static Token getHttpAccessToken(String code) {
        Result<Token> result = Oauth2Utils.getAccessToken(properties.getServerUrl(), properties.getClientId(),
                properties.getClientSecret(), code, getLocalUrl() + properties.getLogoutPath());
        if (!result.isSuccess()) {
            logger.error("getHttpAccessToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
    }

    /**
     * 获取当前应用访问路径
     *
     * @return
     */
    public static String getLocalUrl() {
        HttpServletRequest request = ClientContextHolder.getRequest();
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