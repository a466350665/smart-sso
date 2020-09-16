package com.smart.sso.server;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.remoting.caucho.HessianServiceExporter;

import com.smart.sso.client.SmartContainer;
import com.smart.sso.client.filter.ClientFilter;
import com.smart.sso.client.filter.PermissionFilter;
import com.smart.sso.client.filter.SsoFilter;
import com.smart.sso.client.rpc.AuthenticationRpcService;
import com.smart.sso.server.common.LocalTokenManager;
import com.smart.sso.server.common.TokenManager;

@Configuration
public class SmartSsoConfig {

    @Value("${sso.timeout}")
    private int ssoTimeout;
    @Value("${sso.app.code}")
    private String ssoAppCode;
    @Autowired
    private AuthenticationRpcService authenticationRpcService;

    @Bean
    public FilterRegistrationBean<SmartContainer> smartContainer() {
        SmartContainer smartContainer = new SmartContainer();
        smartContainer.setIsServer(true);
        smartContainer.setAuthenticationRpcService(authenticationRpcService);

        // PermissionFilter 为选配功能，如果仅仅需要单点登录，不需要权限控制可不添加该Filter，随之的sso.app.code也不需要配置
        smartContainer.setFilters(new ClientFilter[] {new SsoFilter(), new PermissionFilter(ssoAppCode)});

        FilterRegistrationBean<SmartContainer> registration = new FilterRegistrationBean<>();
        registration.setFilter(smartContainer);
        registration.addUrlPatterns("/admin/*");
        registration.setName("smartContainer");
        registration.setOrder(1);
        return registration;
    }

    @Bean
    public TokenManager tokenManager() {
        TokenManager tk = new LocalTokenManager();
        tk.setTokenTimeout(ssoTimeout);
        return new LocalTokenManager();
    }

    @Bean(name = "/rpc/authenticationRpcService")
    public HessianServiceExporter authenticationRpcService() {
        HessianServiceExporter exporter = new HessianServiceExporter();
        exporter.setService(authenticationRpcService);
        exporter.setServiceInterface(AuthenticationRpcService.class);
        return exporter;
    }
}
