package openjoe.smart.sso.client;

import openjoe.smart.sso.base.BaseAutoConfiguration;
import openjoe.smart.sso.client.filter.AbstractClientFilter;
import openjoe.smart.sso.client.filter.LoginFilter;
import openjoe.smart.sso.client.filter.LogoutFilter;
import openjoe.smart.sso.client.filter.PermissionFilter;
import openjoe.smart.sso.client.token.TokenPermissionStorage;
import openjoe.smart.sso.client.token.TokenStorage;
import openjoe.smart.sso.client.token.local.LocalTokenPermissionStorage;
import openjoe.smart.sso.client.token.local.LocalTokenStorage;
import openjoe.smart.sso.client.util.SSOUtils;
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
    public TokenStorage tokenStorage(ClientProperties properties) {
        TokenStorage tokenStorage = new LocalTokenStorage();
        TokenPermissionStorage tokenPermissionStorage = new LocalTokenPermissionStorage();
        SSOUtils.setTokenStorage(properties, tokenStorage, tokenPermissionStorage);
        return tokenStorage;
    }

    @Bean
    @ConditionalOnMissingBean(name = "logoutFilter")
    public AbstractClientFilter logoutFilter(ClientProperties properties, TokenStorage tokenStorage) {
        return new LogoutFilter(properties, tokenStorage);
    }

    @Bean
    @ConditionalOnMissingBean(name = "loginFilter")
    public AbstractClientFilter loginFilter(ClientProperties properties) {
        return new LoginFilter(properties);
    }

    @Bean
    @ConditionalOnMissingBean(name = "permissionFilter")
    public AbstractClientFilter permissionFilter(ClientProperties properties) {
        return new PermissionFilter(properties);
    }

    @Bean
    public FilterRegistrationBean<ClientContainer> clientContainer(ClientProperties properties, AbstractClientFilter[] clientFilters) {
        ClientContainer clientContainer = new ClientContainer(properties.getExcludeUrls(), clientFilters);

        FilterRegistrationBean<ClientContainer> registration = new FilterRegistrationBean<>();
        registration.setFilter(clientContainer);
        registration.addUrlPatterns(properties.getUrlPatterns());
        registration.setOrder(properties.getOrder());
        registration.setName(properties.getName());
        return registration;
    }
}