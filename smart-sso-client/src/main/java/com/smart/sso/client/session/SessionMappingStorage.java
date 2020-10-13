package com.smart.sso.client.session;

import javax.servlet.http.HttpSession;

public interface SessionMappingStorage {

    HttpSession removeSessionByMappingId(String mappingId);

    void removeBySessionById(String sessionId);

    void addSessionById(String mappingId, HttpSession session);
}
