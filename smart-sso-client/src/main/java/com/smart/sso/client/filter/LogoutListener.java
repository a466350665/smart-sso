package com.smart.sso.client.filter;

import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import com.smart.sso.client.session.SessionMappingStorage;

/**
 * 单点登出Listener
 * 
 * @author Joe
 */
public final class LogoutListener implements HttpSessionListener {

    private SessionMappingStorage sessionMappingStorage = LogoutFilter.getSessionMappingStorage();

    @Override
    public void sessionCreated(final HttpSessionEvent event) {
    }

    @Override
    public void sessionDestroyed(final HttpSessionEvent event) {
        final HttpSession session = event.getSession();
        sessionMappingStorage.removeBySessionById(session.getId());
    }
}
