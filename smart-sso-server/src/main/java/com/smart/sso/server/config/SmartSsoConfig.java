package com.smart.sso.server.config;

import javax.servlet.http.HttpSessionListener;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.smart.sso.client.SmartContainer;
import com.smart.sso.client.filter.LoginFilter;
import com.smart.sso.client.filter.LogoutFilter;
import com.smart.sso.client.filter.LogoutListener;
import com.smart.sso.server.session.AccessTokenManager;
import com.smart.sso.server.session.CodeManager;
import com.smart.sso.server.session.RefreshTokenManager;
import com.smart.sso.server.session.TicketGrantingTicketManager;
import com.smart.sso.server.session.local.LocalAccessTokenManager;
import com.smart.sso.server.session.local.LocalCodeManager;
import com.smart.sso.server.session.local.LocalRefreshTokenManager;
import com.smart.sso.server.session.local.LocalTicketGrantingTicketManager;

@Configuration
public class SmartSsoConfig {

    @Value("${sso.server.url}")
    private String serverUrl;
    @Value("${sso.app.id}")
    private String appId;
    @Value("${sso.app.secret}")
    private String appSecret;
    @Value("${sso.timeout}")
    private int timeout;
    
	/**
	 * 单点登出Listener
	 * 
	 * @return
	 */
	@Bean
	public ServletListenerRegistrationBean<HttpSessionListener> LogoutListener() {
		ServletListenerRegistrationBean<HttpSessionListener> listenerRegBean = new ServletListenerRegistrationBean<>();
		listenerRegBean.setListener(new LogoutListener());
		return listenerRegBean;
	}

	/**
	 * 单点登录Filter容器
	 * 
	 * @return
	 */
	@Bean
	public FilterRegistrationBean<SmartContainer> smartContainer() {
		SmartContainer smartContainer = new SmartContainer();
		smartContainer.setServerUrl(serverUrl);
		smartContainer.setAppId(appId);
		smartContainer.setAppSecret(appSecret);
		
		// 忽略拦截URL,多个逗号分隔
        smartContainer.setExcludeUrls("/login,/logout,/oauth2/*,/userinfo,/custom/*,/assets/*");

		smartContainer.setFilters(new LogoutFilter(), new LoginFilter());

		FilterRegistrationBean<SmartContainer> registration = new FilterRegistrationBean<>();
		registration.setFilter(smartContainer);
		registration.addUrlPatterns("/*");
		registration.setName("smartContainer");
		registration.setOrder(2);
		return registration;
	}

	/**
	 * 授权码管理器
	 * 
	 * @return
	 */
	@Bean
	public CodeManager codeManager() {
		return new LocalCodeManager();
	}
	
	/**
	 * 调用凭证管理器
	 * 
	 * @return
	 */
	@Bean
	public AccessTokenManager accessTokenManager() {
		// AccessToken时效为登录session时效的1/2
		return new LocalAccessTokenManager(timeout / 2);
	}
	
	/**
	 * 刷新凭证管理器
	 * 
	 * @return
	 */
	@Bean
	public RefreshTokenManager refreshTokenManager() {
		return new LocalRefreshTokenManager(timeout);
	}

	/**
	 * 登录凭证管理器
	 * 
	 * @return
	 */
	@Bean
	public TicketGrantingTicketManager ticketGrantingTicketManager() {
		return new LocalTicketGrantingTicketManager(timeout);
	}
}
