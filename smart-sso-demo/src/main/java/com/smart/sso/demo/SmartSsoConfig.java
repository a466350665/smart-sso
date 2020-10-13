package com.smart.sso.demo;

import javax.servlet.http.HttpSessionListener;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.smart.sso.client.SmartContainer;
import com.smart.sso.client.filter.ClientFilter;
import com.smart.sso.client.filter.LogoutFilter;
import com.smart.sso.client.filter.LogoutListener;
import com.smart.sso.client.filter.PermissionFilter;
import com.smart.sso.client.filter.SsoFilter;

@Configuration
public class SmartSsoConfig {

    @Value("${sso.server.url}")
    private String ssoServerUrl;
    @Value("${sso.app.code}")
    private String ssoAppCode;
    
    @Bean
    public ServletListenerRegistrationBean<HttpSessionListener> sessionListenerWithMetrics() {
        ServletListenerRegistrationBean<HttpSessionListener> listenerRegBean = new ServletListenerRegistrationBean<>();
        listenerRegBean.setListener(new LogoutListener());
        return listenerRegBean;
    }
    
    @Bean
    public FilterRegistrationBean<SmartContainer> smartContainer() {
        SmartContainer smartContainer = new SmartContainer();
        smartContainer.setSsoServerUrl(ssoServerUrl);

        // 忽略登录URL,多个逗号分隔
        // smartContainer.setExcludeUrls("/login,/h5/*");
        
        // PermissionFilter 为选配功能，如果仅仅需要单点登录登出，不需要权限控制可不添加该Filter，随之的sso.app.code也不需要配置
        smartContainer.setFilters(new ClientFilter[]{new LogoutFilter(), new SsoFilter(), new PermissionFilter(ssoAppCode)});

        FilterRegistrationBean<SmartContainer> registration = new FilterRegistrationBean<>();
        registration.setFilter(smartContainer);
        registration.addUrlPatterns("/*");
        registration.setName("smartContainer");
        registration.setOrder(1);
        return registration;
    }
}
