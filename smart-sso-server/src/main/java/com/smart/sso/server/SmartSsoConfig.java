package com.smart.sso.server;

import javax.servlet.http.HttpSessionListener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.remoting.caucho.HessianServiceExporter;

import com.smart.sso.client.SmartContainer;
import com.smart.sso.client.filter.ClientFilter;
import com.smart.sso.client.filter.LogoutFilter;
import com.smart.sso.client.filter.LogoutListener;
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

    @Autowired
    private AuthenticationRpcService authenticationRpcService;
    
    /**
     * 单点登出Listener
     * 
     * @return
     */
    @Bean
    public ServletListenerRegistrationBean<HttpSessionListener> sessionListenerWithMetrics() {
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
        smartContainer.setIsServer(true);
        smartContainer.setAuthenticationRpcService(authenticationRpcService);

        // 忽略登录URL,多个逗号分隔
        // smartContainer.setExcludeUrls("/login,/h5/*");

        // PermissionFilter 为选配功能，如果仅仅需要单点登录登出，不需要权限控制可不添加该Filter，随之的sso.app.code也不需要配置
        smartContainer.setFilters(new ClientFilter[] {new LogoutFilter(), new SsoFilter(), new PermissionFilter(ssoAppCode)});

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

    /**
     * ST校验及权限信息RPC服务
     * 
     * @return
     */
    @Bean(name = "/rpc/authenticationRpcService")
    public HessianServiceExporter authenticationRpcService() {
        HessianServiceExporter exporter = new HessianServiceExporter();
        exporter.setService(authenticationRpcService);
        exporter.setServiceInterface(AuthenticationRpcService.class);
        return exporter;
    }
}
