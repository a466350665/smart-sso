package openjoe.smart.sso.client.util;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.*;
import openjoe.smart.sso.base.util.CookieUtils;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.token.TokenPermissionStorage;
import openjoe.smart.sso.client.token.TokenStorage;
import openjoe.smart.sso.client.token.TokenWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.Optional;

/**
 * SSO工具
 *
 * @author Joe
 */
public class SSOUtils {

    private static final Logger logger = LoggerFactory.getLogger(SSOUtils.class);

    private static ClientProperties properties;
    private static TokenStorage tokenStorage;
    private static TokenPermissionStorage tokenPermissionStorage;

    public static void setTokenStorage(ClientProperties cp, TokenStorage ts, TokenPermissionStorage tps) {
        properties = cp;
        tokenStorage = ts;
        tokenPermissionStorage = tps;
    }

    /**
     * 获取当前登录用户信息
     *
     * @return
     */
    public static TokenUser getUser() {
        return Optional.ofNullable(getTokenWrapper()).map(wrapper -> wrapper.getObject().getTokenUser()).orElse(null);
    }

    /**
     * 获取当前token包装器
     *
     * @return
     */
    public static TokenWrapper getTokenWrapper() {
        String accessToken = getAccessToken();
        if (!StringUtils.hasLength(accessToken)) {
            return null;
        }
        return tokenStorage.get(accessToken);
    }

    /**
     * 获取当前登录用户id
     *
     * @return
     */
    public static Long getUserId() {
        return Optional.ofNullable(getUser()).map(u -> u.getId()).orElse(null);
    }

    /**
     * 获取当前登录用户权限信息
     *
     * @return
     */
    public static TokenPermission getPermission() {
        String accessToken = getAccessToken();
        if (!StringUtils.hasLength(accessToken)) {
            return null;
        }
        return getTokenPermission(accessToken);
    }

    /**
     * 获取未过期的用户权限信息
     *
     * @return
     */
    private static TokenPermission getTokenPermission(String accessToken) {
        ExpirationWrapper<TokenPermission> wrapper = tokenPermissionStorage.get(accessToken);
        if (wrapper == null || wrapper.checkExpired()) {
            return null;
        }
        return wrapper.getObject();
    }

    /**
     * 写入accessToken到cookie
     *
     * @param accessToken
     */
    private static void addCookieAccessToken(String accessToken) {
        CookieUtils.addCookie(properties.getTokenName(), accessToken, "/", ClientContextHolder.getRequest(), ClientContextHolder.getResponse());
    }

    /**
     * 获取当前accessToken
     *
     * @return
     */
    private static String getAccessToken() {
        String accessToken = getCookieAccessToken();
        if (StringUtils.hasLength(accessToken)) {
            return accessToken;
        }
        return getHeaderAccessToken();
    }

    /**
     * 从Cookie中获取当前accessToken
     *
     * @return
     */
    private static String getCookieAccessToken() {
        return CookieUtils.getCookieValue(properties.getTokenName(), ClientContextHolder.getRequest());
    }

    /**
     * 从header中获取当前accessToken
     *
     * @return
     */
    private static String getHeaderAccessToken() {
        return ClientContextHolder.getRequest().getHeader(properties.getTokenName());
    }

    /**
     * 发送http请求获取accessToken，并写入cookie
     *
     * @param code
     * @return
     */
    public static Result<Token> getHttpAccessTokenInCookie(String code) {
        Result<Token> result = getHttpAccessToken(code);
        if (result.isSuccess()) {
            // 写入cookie
            addCookieAccessToken(result.getData().getAccessToken());
        }
        return result;
    }

    /**
     * 发送http请求获取accessToken
     *
     * @param code
     */
    public static Result<Token> getHttpAccessToken(String code) {
        Result<Token> result = OAuth2Utils.getAccessToken(properties.getServerUrl(), properties.getClientId(),
                properties.getClientSecret(), code, getLocalUrl() + properties.getLogoutPath());
        if (result.isSuccess()) {
            Token token = result.getData();
            // 将token存储到本地
            tokenStorage.create(token);
            // 用accessToken请求用户权限信息，存储到本地
            TokenPermission tokenPermission = getHttpTokenPermission(token.getAccessToken());
            tokenPermissionStorage.create(token, tokenPermission);
        } else {
            logger.error("getHttpAccessToken has error, message:{}", result.getMessage());
        }
        return result;
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
     * 刷新token
     *
     * @param wrapper
     * @return
     */
    public static boolean refreshToken(TokenWrapper wrapper) {
        Result<Token> result = getHttpRefreshTokenInCookie(wrapper.getObject().getRefreshToken());
        if (result.isSuccess()) {
            // 更新当前Request中Cookie的token值，让之后的请求能在Cookie中拿到新的token值。
            CookieUtils.updateCookie(properties.getTokenName(), result.getData().getAccessToken(), ClientContextHolder.getRequest());
            return true;
        }
        return false;
    }

    /**
     * 发送http请求刷新token，并写入cookie
     *
     * @param refreshToken
     * @return
     */
    public static Result<Token> getHttpRefreshTokenInCookie(String refreshToken) {
        Result<Token> result = getHttpRefreshToken(refreshToken);
        if (result.isSuccess()) {
            // Response写入cookie
            addCookieAccessToken(result.getData().getAccessToken());
        }
        return result;
    }

    /**
     * 发送http请求刷新token
     *
     * @param refreshToken
     * @return
     */
    public static Result<Token> getHttpRefreshToken(String refreshToken) {
        String accessToken = tokenStorage.getAccessToken(refreshToken);
        if (!StringUtils.hasLength(accessToken)) {
            return Result.error("accessToken is null or expired!");
        }
        TokenPermission tokenPermission = getTokenPermission(accessToken);
        if (tokenPermission == null) {
            return Result.error("tokenPermission is null or expired!");
        }
        Result<Token> result = OAuth2Utils.getRefreshToken(properties.getServerUrl(), properties.getClientId(), refreshToken);
        if (result.isSuccess()) {
            // 删除旧token
            tokenStorage.remove(accessToken);
            // 删除旧用户权限信息
            tokenPermissionStorage.remove(accessToken);

            // 将token存储到本地
            tokenStorage.create(result.getData());
            // 存储用户权限信息到本地
            tokenPermissionStorage.create(result.getData(), tokenPermission);
        } else {
            logger.error("getHttpRefreshToken has error, message:{}", result.getMessage());
        }
        return result;
    }

    /**
     * 发送http请求用户权限信息
     *
     * @param accessToken
     */
    private static TokenPermission getHttpTokenPermission(String accessToken) {
        Result<TokenPermission> result = PermissionUtils.getUserPermission(properties.getServerUrl(), accessToken);
        if (!result.isSuccess()) {
            logger.error("getHttpTokenPermission has error, message:{}", result.getMessage());
            return new TokenPermission(Collections.emptySet(), Collections.emptySet(), Collections.emptyList());
        }
        return result.getData();
    }

    /**
     * 构建登录url
     *
     * @param redirectUri
     * @return
     */
    public static String buildLoginUrl(String redirectUri) {
        try {
            redirectUri = URLEncoder.encode(redirectUri, "utf-8");
        } catch (UnsupportedEncodingException e) {
            logger.error("buildLoginUrl has error, message:{}", e.getMessage());
        }
        return new StringBuilder()
                .append(properties.getServerUrl())
                .append(BaseConstant.LOGIN_PATH)
                .append("?")
                .append(BaseConstant.CLIENT_ID)
                .append("=")
                .append(properties.getClientId())
                .append("&")
                .append(BaseConstant.REDIRECT_URI)
                .append("=")
                .append(redirectUri).toString();
    }

    /**
     * 构建退出url
     *
     * @return
     */
    public static String buildLogoutUrl() {
        return buildLogoutUrl(getLocalUrl());
    }

    /**
     * 构建退出url
     *
     * @param redirectUri
     * @return
     */
    public static String buildLogoutUrl(String redirectUri) {
        try {
            redirectUri = URLEncoder.encode(redirectUri, "utf-8");
        } catch (UnsupportedEncodingException e) {
            logger.error("buildLogoutUrl has error, message:{}", e.getMessage());
        }
        return new StringBuilder()
                .append(properties.getServerUrl())
                .append(BaseConstant.LOGOUT_PATH)
                .append("?")
                .append(BaseConstant.REDIRECT_URI)
                .append("=")
                .append(redirectUri).toString();
    }
}