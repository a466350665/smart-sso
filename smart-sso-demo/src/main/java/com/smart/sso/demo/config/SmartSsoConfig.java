package com.smart.sso.demo.config;

import javax.servlet.http.HttpSessionListener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.smart.sso.client.SmartContainer;
import com.smart.sso.client.filter.LoginFilter;
import com.smart.sso.client.filter.LogoutFilter;
import com.smart.sso.client.listener.LogoutListener;
import com.smart.sso.client.session.SessionMappingStorage;
import com.smart.sso.client.session.local.LocalSessionMappingStorage;

@Configuration
public class SmartSsoConfig {

    @Value("${sso.server.url}")
    private String serverUrl;
    @Value("${sso.app.id}")
    private String appId;
    @Value("${sso.app.secret}")
    private String appSecret;
    
    @Autowired
    private SessionMappingStorage sessionMappingStorage;
    
	/**
	 * 存储session和accessToken映射
	 * @return
	 */
	@Bean
    public SessionMappingStorage sessionMappingStorage() {
		return new LocalSessionMappingStorage();
    }
    
	/**
	 * 单实例方式单点登出Listener
	 * 
	 * @return
	 */
	@Bean
	public ServletListenerRegistrationBean<HttpSessionListener> LogoutListener() {
		ServletListenerRegistrationBean<HttpSessionListener> listenerRegBean = new ServletListenerRegistrationBean<>();
		LogoutListener logoutListener = new LogoutListener();
		logoutListener.setSessionMappingStorage(sessionMappingStorage);
		listenerRegBean.setListener(logoutListener);
		return listenerRegBean;
	}

	/**
	 * 分布式redis方式单点登出Listener
	 * 
	 * @return
	 */
//	@Bean
//	public ApplicationListener<AbstractSessionEvent> LogoutListener() {
//		List<HttpSessionListener> httpSessionListeners = new ArrayList<>();
//		LogoutListener logoutListener = new LogoutListener();
//		logoutListener.setSessionMappingStorage(sessionMappingStorage);
//		httpSessionListeners.add(logoutListener);
//		return new SessionEventHttpSessionListenerAdapter(httpSessionListeners);
//	}
    
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
		smartContainer.setSessionMappingStorage(sessionMappingStorage);

        // 忽略拦截URL,多个逗号分隔
        // smartContainer.setExcludeUrls("/login,/h5/*");
        
        smartContainer.setFilters(new LogoutFilter(), new LoginFilter());

        FilterRegistrationBean<SmartContainer> registration = new FilterRegistrationBean<>();
        registration.setFilter(smartContainer);
        registration.addUrlPatterns("/*");
        registration.setName("smartContainer");
        return registration;
    }
}
