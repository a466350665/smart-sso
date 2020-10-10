package com.smart.sso.client.filter;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.session.HashMapBackedSessionMappingStorage;
import com.smart.sso.client.session.SessionMappingStorage;

/**
 * 单点登出Filter
 * 
 * @author Joe
 */
public class LogoutFilter extends ParamFilter implements Filter {

    private static SessionMappingStorage sessionMappingStorage = new HashMapBackedSessionMappingStorage();

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {}

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain chain)
        throws IOException, ServletException {
        final HttpServletRequest request = (HttpServletRequest)servletRequest;
        final HttpServletResponse response = (HttpServletResponse)servletResponse;

        String token = request.getParameter(SsoConstant.TICKET);
        if (token != null) {
            recordSession(request, token);
            chain.doFilter(request, response);
        }

        token = request.getParameter(SsoConstant.LOGOUT_PARAMETER_NAME);
        if (token != null) {
            destroySession(request, token);
        }

        chain.doFilter(request, response);
    }

    private void recordSession(final HttpServletRequest request, String token) {
        final HttpSession session = request.getSession();
        try {
            sessionMappingStorage.removeBySessionById(session.getId());
        } catch (Exception e) {
        }
        sessionMappingStorage.addSessionById(token, session);
    }

    private void destroySession(final HttpServletRequest request, String token) {
        final HttpSession session = sessionMappingStorage.removeSessionByMappingId(token);

        if (session != null) {
            try {
                session.invalidate();
            } catch (IllegalStateException e) {
            }
        }
    }

    public static SessionMappingStorage getSessionMappingStorage() {
        return sessionMappingStorage;
    }

    @Override
    public void destroy() {}
}