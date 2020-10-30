package com.smart.sso.client.session;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpSession;

/**
 * 借鉴CAS
 * 
 * @author Joe
 */
public final class LocalSessionMappingStorage implements SessionMappingStorage {

    private final Map<String, HttpSession> tokenSessionMap = new HashMap<>();
    private final Map<String, String> sessionTokenMap = new HashMap<>();

    @Override
    public synchronized void addSessionById(final String accessToken, final HttpSession session) {
        sessionTokenMap.put(session.getId(), accessToken);
        tokenSessionMap.put(accessToken, session);

    }

    @Override
    public synchronized void removeBySessionById(final String sessionId) {
        final String accessToken = sessionTokenMap.get(sessionId);
        tokenSessionMap.remove(accessToken);
        sessionTokenMap.remove(sessionId);
    }

    @Override
    public synchronized HttpSession removeSessionByMappingId(final String accessToken) {
        final HttpSession session = tokenSessionMap.get(accessToken);
        if (session != null) {
            removeBySessionById(session.getId());
        }
        return session;
    }
}
