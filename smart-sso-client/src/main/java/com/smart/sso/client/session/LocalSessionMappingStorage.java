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

    private final Map<String, HttpSession> MANAGED_SESSIONS = new HashMap<>();

    private final Map<String, String> ID_TO_SESSION_KEY_MAPPING = new HashMap<>();

    @Override
    public synchronized void addSessionById(final String mappingId, final HttpSession session) {
        ID_TO_SESSION_KEY_MAPPING.put(session.getId(), mappingId);
        MANAGED_SESSIONS.put(mappingId, session);

    }

    @Override
    public synchronized void removeBySessionById(final String sessionId) {
        final String key = ID_TO_SESSION_KEY_MAPPING.get(sessionId);

        MANAGED_SESSIONS.remove(key);
        ID_TO_SESSION_KEY_MAPPING.remove(sessionId);
    }

    @Override
    public synchronized HttpSession removeSessionByMappingId(final String mappingId) {
        final HttpSession session = MANAGED_SESSIONS.get(mappingId);

        if (session != null) {
            removeBySessionById(session.getId());
        }

        return session;
    }
}
