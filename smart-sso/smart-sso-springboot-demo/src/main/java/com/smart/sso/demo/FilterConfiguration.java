package com.smart.sso.demo;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.smart.sso.client.SmartContainer;
import com.smart.sso.client.filter.ClientFilter;
import com.smart.sso.client.filter.PermissionFilter;
import com.smart.sso.client.filter.SsoFilter;

@Configuration
public class FilterConfiguration {

    @Value("${sso.server.url}")
    private String ssoServerUrl;
    @Value("${sso.app.code}")
    private String ssoAppCode;

    @Bean
    public FilterRegistrationBean<SmartContainer> testFilterRegistration() {
        SmartContainer smartContainer = new SmartContainer();
        smartContainer.setSsoServerUrl(ssoServerUrl);
        // PermissionFilter 为选配功能，如果仅仅需要单点登录，不需要权限控制可不添加该Filter，随之的sso.app.code也不需要配置
        smartContainer.setFilters(new ClientFilter[]{new SsoFilter(), new PermissionFilter(ssoAppCode)});

        FilterRegistrationBean<SmartContainer> registration = new FilterRegistrationBean<>();
        registration.setFilter(smartContainer);
        registration.addUrlPatterns("/*");
        registration.setName("smartContainer");
        registration.setOrder(1);
        return registration;
    }
}
