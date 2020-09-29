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
import com.smart.sso.server.common.LocalServiceTicketManager;
import com.smart.sso.server.common.LocalTicketGrantingTicketManager;
import com.smart.sso.server.common.ServiceTicketManager;
import com.smart.sso.server.common.TicketGrantingTicketManager;

@Configuration
public class SmartSsoConfig {

    @Value("${sso.server.url}")
    private String ssoServerUrl;
    @Value("${sso.app.code}")
    private String ssoAppCode;
    @Value("${sso.logout.backUrl}")
    private String backUrl;

    @Autowired
    private AuthenticationRpcService authenticationRpcService;
    
    @Bean
    public FilterRegistrationBean<SmartContainer> smartContainer() {
        SmartContainer smartContainer = new SmartContainer();
        smartContainer.setIsServer(true);
        smartContainer.setAuthenticationRpcService(authenticationRpcService);
        
        // 忽略登录URL,多个逗号分隔
        // smartContainer.setExcludeUrls("/login,/h5/*");

        // PermissionFilter 为选配功能，如果仅仅需要单点登录，不需要权限控制可不添加该Filter，随之的sso.app.code也不需要配置
        smartContainer.setFilters(new ClientFilter[] {new SsoFilter(), new PermissionFilter(ssoAppCode)});

        FilterRegistrationBean<SmartContainer> registration = new FilterRegistrationBean<>();
        registration.setFilter(smartContainer);
        registration.addUrlPatterns("/admin/*");
        registration.setName("smartContainer");
        registration.setOrder(2);
        return registration;
    }

    /**
     * 票据ST管理器
     * @return
     */
    @Bean
    public ServiceTicketManager serviceTicketManager() {
        return new LocalServiceTicketManager();
    }
    
    /**
     * 令牌TGT管理器
     * @return
     */
    @Bean
    public TicketGrantingTicketManager ticketGrantingTicketManager() {
        return new LocalTicketGrantingTicketManager();
    }

    @Bean(name = "/rpc/authenticationRpcService")
    public HessianServiceExporter authenticationRpcService() {
        HessianServiceExporter exporter = new HessianServiceExporter();
        exporter.setService(authenticationRpcService);
        exporter.setServiceInterface(AuthenticationRpcService.class);
        return exporter;
    }
}
