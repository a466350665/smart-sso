package com.smart.sso.client.filter;

import java.io.IOException;
import java.net.URLEncoder;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.smart.sso.client.util.SessionUtils;

/**
 * 单点登出Filter
 * 
 * @author Joe
 */
public class LogoutFilter extends ParamFilter implements Filter {
    
    /**
     * 登出后，回调地址
     */
    private String backUrl;
    
    public LogoutFilter() {
    }

    public LogoutFilter(String ssoServerUrl, String backUrl) {
        this.ssoServerUrl = ssoServerUrl;
        this.backUrl = backUrl;
    }
    
    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        if (ssoServerUrl == null || ssoServerUrl.isEmpty()) {
            throw new IllegalArgumentException("ssoServerUrl不能为空");
        }
        if (backUrl == null || backUrl.isEmpty()) {
            throw new IllegalArgumentException("backUrl不能为空");
        }
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
        throws IOException, ServletException {
        // 清除本地session
        SessionUtils.invalidate((HttpServletRequest) request);
        
        // 跳转至服务端退出
        String ssoLoginUrl = new StringBuilder().append(ssoServerUrl).append("/logout?service=")
            .append(URLEncoder.encode(backUrl, "utf-8")).toString();

        ((HttpServletResponse) response).sendRedirect(ssoLoginUrl);
    }
    
    public void setBackUrl(String backUrl) {
        this.backUrl = backUrl;
    }

    @Override
    public void destroy() {
    }
}