package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.ClientContextHolder;
import openjoe.smart.sso.client.util.TokenUtils;
import org.springframework.core.annotation.Order;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URLEncoder;

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
        Token token = TokenUtils.getAndRefresh();
        // 本地已存在token，直接返回
        if (token != null) {
            return true;
        }
        HttpServletRequest request = ClientContextHolder.getRequest();
        String code = request.getParameter(BaseConstant.AUTH_CODE);
        // 携带授权码请求
        if (code != null && (token = TokenUtils.getHttpAccessToken(code)) != null) {
            // 将token存储到本地
            TokenUtils.setInCookie(token);
            // 为去除URL中授权码参数，再跳转一次当前地址
            redirectLocalRemoveCode(request);
        } else {
            // 跳转至服务端登录
            redirectLogin(request);
        }
        return false;
    }

    /**
     * 跳转至服务端登录
     *
     * @param request
     * @throws IOException
     */
    private void redirectLogin(HttpServletRequest request) throws IOException {
        if (isAjaxRequest(request)) {
            responseJson(ClientConstant.NO_LOGIN, "未登录或已超时");
        } else {
            String loginUrl = buildLoginUrl(request);
            ClientContextHolder.getResponse().sendRedirect(loginUrl);
        }
    }

    private String buildLoginUrl(HttpServletRequest request)  throws IOException {
        return new StringBuilder()
                .append(getProperties().getServerUrl())
                .append(BaseConstant.LOGIN_PATH)
                .append("?")
                .append(BaseConstant.CLIENT_ID)
                .append("=")
                .append(getProperties().getClientId())
                .append("&")
                .append(BaseConstant.REDIRECT_URI)
                .append("=")
                .append(URLEncoder.encode(getCurrentUrl(request), "utf-8")).toString();
    }

    /**
     * 去除返回地址中的票据参数
     *
     * @return
     * @throws IOException
     */
    private void redirectLocalRemoveCode(HttpServletRequest request) throws IOException {
        String currentUrl = getCurrentUrl(request);
        currentUrl = currentUrl.substring(0, currentUrl.indexOf(BaseConstant.AUTH_CODE) - 1);
        ClientContextHolder.getResponse().sendRedirect(currentUrl);
    }

    /**
     * 获取当前请求地址
     *
     * @param request
     * @return
     */
    private String getCurrentUrl(HttpServletRequest request) {
        StringBuilder urlBuilder = new StringBuilder(request.getRequestURL());
        String queryString = request.getQueryString();
        if (queryString != null) {
            urlBuilder.append("?").append(queryString);
        }
        return urlBuilder.toString();
    }

    protected boolean isAjaxRequest(HttpServletRequest request) {
        return "XMLHttpRequest".equals(request.getHeader("X-Requested-With"));
    }

    public void setProperties(ClientProperties properties) {
        this.properties = properties;
    }

    public ClientProperties getProperties() {
        return properties;
    }
}