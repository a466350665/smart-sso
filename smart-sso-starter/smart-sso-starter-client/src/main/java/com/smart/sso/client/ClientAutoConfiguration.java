package com.smart.sso.client;

import com.smart.sso.base.BaseAutoConfiguration;
import com.smart.sso.client.filter.LoginFilter;
import com.smart.sso.client.filter.LogoutFilter;
import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.local.LocalTokenStorage;
import com.smart.sso.client.util.TokenUtils;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * 客户端装配
 *
 * @author Joe
 */
@Configuration(proxyBeanMethods = false)
@AutoConfigureBefore({BaseAutoConfiguration.class})
@EnableConfigurationProperties({ClientProperties.class})
public class ClientAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(TokenStorage.class)
    public TokenStorage tokenStorage() {
        TokenStorage ts = new LocalTokenStorage();
        TokenUtils.setTokenStorage(ts);
        return ts;
    }

    @Bean
    public FilterRegistrationBean<ClientContainer> clientContainer(ClientProperties properties, TokenStorage tokenStorage) {
        ClientContainer clientContainer = new ClientContainer(properties, tokenStorage);
        clientContainer.setFilters(new LogoutFilter(), new LoginFilter());

        FilterRegistrationBean<ClientContainer> registration = new FilterRegistrationBean<>();
        registration.setFilter(clientContainer);
        registration.addUrlPatterns(properties.getUrlPatterns());
        registration.setOrder(properties.getOrder());
        registration.setName(properties.getName());
        return registration;
    }
}