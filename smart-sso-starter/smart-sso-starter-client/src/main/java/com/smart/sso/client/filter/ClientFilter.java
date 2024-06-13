package com.smart.sso.client.filter;

import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.token.TokenStorage;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Filter基类
 *
 * @author Joe
 */
public abstract class ClientFilter implements Filter {

    private TokenStorage tokenStorage;

    protected ClientProperties properties;

    public abstract boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response)
            throws IOException;

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException {
    }

    @Override
    public void destroy() {
    }

    public TokenStorage getTokenStorage() {
        return tokenStorage;
    }

    public void setTokenStorage(TokenStorage tokenStorage) {
        this.tokenStorage = tokenStorage;
    }

    public void setProperties(ClientProperties properties) {
        this.properties = properties;
    }
}