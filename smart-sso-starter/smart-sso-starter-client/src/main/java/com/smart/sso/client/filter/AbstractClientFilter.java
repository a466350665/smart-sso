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
public abstract class AbstractClientFilter {

    private TokenStorage tokenStorage;

    private ClientProperties properties;

    /**
     * 请求是否允许通过
     *
     * @param request
     * @param response
     * @return
     * @throws IOException
     */
    public abstract boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response)
            throws IOException;

    public TokenStorage getTokenStorage() {
        return tokenStorage;
    }

    public void setTokenStorage(TokenStorage tokenStorage) {
        this.tokenStorage = tokenStorage;
    }

    public void setProperties(ClientProperties properties) {
        this.properties = properties;
    }

    public ClientProperties getProperties() {
        return properties;
    }
}