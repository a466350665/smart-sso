package openjoe.smart.sso.client.util;

import openjoe.smart.sso.base.constant.BaseConstant;
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
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Optional;

/**
 * Token工具
 *
 * @author Joe
 */
public class SSOUtils {

    private static final Logger logger = LoggerFactory.getLogger(SSOUtils.class);

    private static ClientProperties properties;
    private static TokenStorage tokenStorage;

    public static void setTokenStorage(ClientProperties cp, TokenStorage ts) {
        properties = cp;
        tokenStorage = ts;
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
     * 获取当前登录用户信息
     *
     * @return
     */
    public static TokenUser getUser() {
        return Optional.ofNullable(getTokenWrapper()).map(wrapper -> wrapper.getObject().getTokenUser()).orElse(null);
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
        return Optional.ofNullable(getTokenWrapper()).map(wrapper -> wrapper.getObject().getTokenPermission()).orElse(null);
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
        Result<Token> result = Oauth2Utils.getAccessToken(properties.getServerUrl(), properties.getClientId(),
                properties.getClientSecret(), code, getLocalUrl() + properties.getLogoutPath());
        if (result.isSuccess()) {
            // 将token存储到本地
            tokenStorage.create(result.getData());
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
            // 删除旧token
            tokenStorage.remove(wrapper.getObject().getAccessToken());

            Token token = result.getData();
            // 更新当前Cookie中的token值
            CookieUtils.updateCookie(properties.getTokenName(), token.getAccessToken(), ClientContextHolder.getRequest());
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
        Result<Token> result = Oauth2Utils.getRefreshToken(properties.getServerUrl(), properties.getClientId(), refreshToken);
        if (result.isSuccess()) {
            // 将token存储到本地
            tokenStorage.create(result.getData());
        } else {
            logger.error("getHttpRefreshToken has error, message:{}", result.getMessage());
        }
        return result;
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
            logger.error("buildLoginUrl has error, message:{}", e.getMessage());
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