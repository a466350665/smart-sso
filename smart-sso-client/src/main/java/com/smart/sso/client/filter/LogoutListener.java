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

    private SessionMappingStorage sessionMappingStorage;

    @Override
    public void sessionCreated(final HttpSessionEvent event) {
    }

    @Override
    public void sessionDestroyed(final HttpSessionEvent event) {
        if (sessionMappingStorage == null) {
            sessionMappingStorage = LogoutFilter.getSessionMappingStorage();
        }
        final HttpSession session = event.getSession();
        sessionMappingStorage.removeBySessionById(session.getId());
    }
}
