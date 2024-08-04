package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.token.TokenWrapper;
import openjoe.smart.sso.client.util.ClientContextHolder;
import openjoe.smart.sso.client.util.SSOUtils;
import org.springframework.core.annotation.Order;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

/**
 * 单点登录Filter
 *
 * @author Joe
 */
@Order(20)
public class LoginFilter extends AbstractClientFilter {

    private ClientProperties properties;

    public LoginFilter(ClientProperties properties) {
        this.properties = properties;
    }

    @Override
    public boolean isAccessAllowed() throws IOException {
        TokenWrapper tokenWrapper = SSOUtils.getTokenWrapper();
        // 本地已存在token
        if (tokenWrapper != null) {
            // 如果accessToken没过期，直接返回
            if (!tokenWrapper.checkExpired()) {
                return true;
            }
            // 如果accessToken已过期，refreshToken没过期
            if (!tokenWrapper.checkRefreshExpired()) {
                // 前后端分离场景，通知客户端使用refresh接口刷新token
                if(properties.getH5Enabled()){
                    responseJson(ClientConstant.NO_TOKEN, "token已失效");
                    return false;
                }
                // 前后端一体化场景，自动刷新token
                else if(SSOUtils.refreshToken(tokenWrapper)){
                    return true;
                }
            }
        }

        String code = ClientContextHolder.getRequest().getParameter(BaseConstant.AUTH_CODE);
        // 携带授权码请求
        if (code != null && SSOUtils.getHttpAccessTokenInCookie(code).isSuccess()){
            // 为去除URL中授权码参数，再跳转一次当前地址
            redirectLocalRemoveCode();
        } else {
            // 跳转至服务端登录
            redirectLogin();
        }
        return false;
    }

    /**
     * 跳转至服务端登录
     *
     * @throws IOException
     */
    private void redirectLogin() throws IOException {
        if (isAjaxRequest()) {
            responseJson(ClientConstant.NO_LOGIN, "未登录或已超时");
        } else {
            String loginUrl = SSOUtils.buildLoginUrl(getCurrentUrl());
            ClientContextHolder.getResponse().sendRedirect(loginUrl);
        }
    }

    /**
     * 获取当前请求地址
     *
     * @return
     */
    public static String getCurrentUrl() {
        HttpServletRequest request = ClientContextHolder.getRequest();
        StringBuilder urlBuilder = new StringBuilder(request.getRequestURL());
        String queryString = request.getQueryString();
        if (queryString != null) {
            urlBuilder.append("?").append(queryString);
        }
        return urlBuilder.toString();
    }

    /**
     * 去除返回地址中的票据参数
     *
     * @return
     * @throws IOException
     */
    private void redirectLocalRemoveCode() throws IOException {
        String currentUrl = getCurrentUrl();
        currentUrl = currentUrl.substring(0, currentUrl.indexOf(BaseConstant.AUTH_CODE) - 1);
        ClientContextHolder.getResponse().sendRedirect(currentUrl);
    }

    /**
     * 判断是否为ajax请求
     * @return
     */
    protected boolean isAjaxRequest() {
        return "XMLHttpRequest".equals(ClientContextHolder.getRequest().getHeader("X-Requested-With"));
    }

    public void setProperties(ClientProperties properties) {
        this.properties = properties;
    }

    public ClientProperties getProperties() {
        return properties;
    }
}