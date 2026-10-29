package openjoe.smart.sso.client;

import openjoe.smart.sso.client.filter.AbstractClientFilter;
import openjoe.smart.sso.client.filter.SsoLoginFilter;
import openjoe.smart.sso.client.filter.SsoLogoutFilter;
import openjoe.smart.sso.client.token.TokenPermissionStorage;
import openjoe.smart.sso.client.token.SsoTokenStorage;
import openjoe.smart.sso.client.token.local.LocalTokenPermissionStorage;
import openjoe.smart.sso.client.token.local.LocalTokenStorage;
import openjoe.smart.sso.client.util.SsoUtils;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

/**
 * 客户端装配
 *
 * @author Joe
 */
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties({ClientProperties.class})
public class ClientAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(SsoTokenStorage.class)
    public SsoTokenStorage ssoTokenStorage(ClientProperties properties) {
        SsoTokenStorage tokenStorage = new LocalTokenStorage();
        TokenPermissionStorage tokenPermissionStorage = new LocalTokenPermissionStorage();
        SsoUtils.setTokenStorage(properties, tokenStorage, tokenPermissionStorage);
        return tokenStorage;
    }

    @Bean
    @ConditionalOnMissingBean(SsoLogoutFilter.class)
    public SsoLogoutFilter ssoLogoutFilter(ClientProperties properties, SsoTokenStorage tokenStorage) {
        return new SsoLogoutFilter(properties, tokenStorage);
    }

    @Bean
    @ConditionalOnMissingBean(SsoLoginFilter.class)
    public SsoLoginFilter ssoLoginFilter(ClientProperties properties) {
        return new SsoLoginFilter(properties);
    }

    @Bean
    @ConditionalOnMissingBean
    public SsoUrlScopeMatcher ssoUrlScopeMatcher(ClientProperties properties) {
        return new SsoUrlScopeMatcher(properties);
    }

    @Bean
    @ConditionalOnMissingBean
    public SsoAuthorizationInterceptor ssoAuthorizationInterceptor(SsoUrlScopeMatcher scopeMatcher) {
        return new SsoAuthorizationInterceptor(scopeMatcher);
    }

    @Bean
    @ConditionalOnMissingBean(name = "ssoAuthorizationWebMvcConfigurer")
    public org.springframework.web.servlet.config.annotation.WebMvcConfigurer ssoAuthorizationWebMvcConfigurer(
            SsoAuthorizationInterceptor interceptor) {
        return new org.springframework.web.servlet.config.annotation.WebMvcConfigurer() {
            @Override
            public void addInterceptors(
                    org.springframework.web.servlet.config.annotation.InterceptorRegistry registry) {
                registry.addInterceptor(interceptor);
            }
        };
    }

    @Bean
    public FilterRegistrationBean<SsoClientContainer> ssoClientContainer(
            ClientProperties properties, SsoUrlScopeMatcher scopeMatcher, AbstractClientFilter[] clientFilters) {
        SsoClientContainer ssoClientContainer = new SsoClientContainer(scopeMatcher, clientFilters);

        FilterRegistrationBean<SsoClientContainer> registration = new FilterRegistrationBean<>();
        registration.setFilter(ssoClientContainer);
        registration.addUrlPatterns(properties.getUrlPatterns());
        registration.setOrder(properties.getOrder());
        registration.setName(properties.getName());
        return registration;
    }

    /**
     * server-url 留空时判定"同源"，并做启动校验
     */
    @Bean
    @ConditionalOnMissingBean
    public SsoClientServerAddressResolver ssoClientServerAddressResolver(
            ClientProperties properties, Environment environment) {
        return new SsoClientServerAddressResolver(properties, environment);
    }
}
