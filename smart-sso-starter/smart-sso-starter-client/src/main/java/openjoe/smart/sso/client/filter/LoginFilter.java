package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.constant.Oauth2Constant;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.util.JsonUtils;
import openjoe.smart.sso.client.ClientProperties;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.TokenUtils;
import org.springframework.core.annotation.Order;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
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
    public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
        Token token = TokenUtils.getAndRefresh(request, response);
        // 本地已存在token，直接返回
        if (token != null) {
            return true;
        }
        String code = request.getParameter(Oauth2Constant.AUTH_CODE);
        // 携带授权码请求
        if (code != null && (token = TokenUtils.getHttpAccessToken(code)) != null) {
            // 将token存储到本地
            TokenUtils.set(token, request, response);
            // 为去除URL中授权码参数，再跳转一次当前地址
            redirectLocalRemoveCode(request, response);
        } else {
            // 跳转至服务端登录
            redirectLogin(request, response);
        }
        return false;
    }

    /**
     * 跳转至服务端登录
     *
     * @param request
     * @param response
     * @throws IOException
     */
    private void redirectLogin(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (isAjaxRequest(request)) {
            responseJson(response, ClientConstant.NO_LOGIN, "未登录或已超时");
        } else {
            String loginUrl = buildLoginUrl(request);
            response.sendRedirect(loginUrl);
        }
    }

    private String buildLoginUrl(HttpServletRequest request)  throws IOException {
        return new StringBuilder()
                .append(getProperties().getServerUrl())
                .append(BaseConstant.LOGIN_PATH)
                .append("?")
                .append(Oauth2Constant.APP_KEY)
                .append("=")
                .append(getProperties().getAppKey())
                .append("&")
                .append(BaseConstant.REDIRECT_URI)
                .append("=")
                .append(URLEncoder.encode(getCurrentUrl(request), "utf-8")).toString();
    }

    /**
     * 去除返回地址中的票据参数
     *
     * @param request
     * @return
     * @throws IOException
     */
    private void redirectLocalRemoveCode(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String currentUrl = getCurrentUrl(request);
        currentUrl = currentUrl.substring(0, currentUrl.indexOf(Oauth2Constant.AUTH_CODE) - 1);
        response.sendRedirect(currentUrl);
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

    protected void responseJson(HttpServletResponse response, int code, String message) throws IOException {
        response.setContentType("application/json;charset=UTF-8");
        response.setStatus(HttpServletResponse.SC_OK);
        try (PrintWriter writer = response.getWriter()) {
            writer.write(JsonUtils.toString(Result.create(code, message)));
        }
    }

    public void setProperties(ClientProperties properties) {
        this.properties = properties;
    }

    public ClientProperties getProperties() {
        return properties;
    }
}