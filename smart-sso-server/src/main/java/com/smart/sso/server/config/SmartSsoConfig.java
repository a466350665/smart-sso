package com.smart.sso.server.config;

import javax.servlet.http.HttpSessionListener;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.smart.sso.client.SmartContainer;
import com.smart.sso.client.filter.LogoutFilter;
import com.smart.sso.client.filter.LogoutListener;
import com.smart.sso.client.filter.SsoFilter;
import com.smart.sso.server.common.LocalServiceTicketManager;
import com.smart.sso.server.common.LocalTicketGrantingTicketManager;
import com.smart.sso.server.common.ServiceTicketManager;
import com.smart.sso.server.common.TicketGrantingTicketManager;

@Configuration
public class SmartSsoConfig {

    @Value("${sso.server.url}")
    private String ssoServerUrl;

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
        smartContainer.setSsoServerUrl(ssoServerUrl);

        smartContainer.setFilters(new LogoutFilter(), new SsoFilter());

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
}
