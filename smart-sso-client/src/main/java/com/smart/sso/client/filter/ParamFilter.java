package com.smart.sso.client.filter;

import com.smart.sso.client.session.SessionMappingStorage;

/**
 * 参数注入Filter
 * 
 * @author Joe
 */
public class ParamFilter {

	protected String appId;
	protected String appSecret;
	protected String serverUrl;
	protected SessionMappingStorage sessionMappingStorage;
	
	public String getAppId() {
		return appId;
	}

	public void setAppId(String appId) {
		this.appId = appId;
	}

	public String getAppSecret() {
		return appSecret;
	}

	public void setAppSecret(String appSecret) {
		this.appSecret = appSecret;
	}

	public String getServerUrl() {
		return serverUrl;
	}

	public void setServerUrl(String serverUrl) {
		this.serverUrl = serverUrl;
	}

	public SessionMappingStorage getSessionMappingStorage() {
		return sessionMappingStorage;
	}

	public void setSessionMappingStorage(SessionMappingStorage sessionMappingStorage) {
		this.sessionMappingStorage = sessionMappingStorage;
	}
}