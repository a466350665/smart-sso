/**
 * Licensed to Apereo under one or more contributor license
 * agreements. See the NOTICE file distributed with this work
 * for additional information regarding copyright ownership.
 * Apereo licenses this file to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain a
 * copy of the License at the following location:
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package com.smart.sso.client.session;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpSession;

/**
 * HashMap backed implementation of SessionMappingStorage.
 * 
 * @author Scott Battaglia
 * @version $Revision$ $Date$
 * @since 3.1
 *
 */
public final class HashMapBackedSessionMappingStorage implements SessionMappingStorage {

    /**
     * Maps the ID from the CAS server to the Session.
     */
    private final Map<String, HttpSession> MANAGED_SESSIONS = new HashMap<String, HttpSession>();

    /**
     * Maps the Session ID to the key from the CAS Server.
     */
    private final Map<String, String> ID_TO_SESSION_KEY_MAPPING = new HashMap<String, String>();

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
