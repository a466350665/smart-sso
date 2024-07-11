package openjoe.smart.sso.server;

import openjoe.smart.sso.base.BaseAutoConfiguration;
import openjoe.smart.sso.server.manager.AbstractCodeManager;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import openjoe.smart.sso.server.manager.local.LocalCodeManager;
import openjoe.smart.sso.server.manager.local.LocalTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.local.LocalTokenManager;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration(proxyBeanMethods = false)
@AutoConfigureBefore({BaseAutoConfiguration.class})
@EnableConfigurationProperties({ServerProperties.class})
public class ServerAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(AbstractCodeManager.class)
    public AbstractCodeManager codeManager(ServerProperties properties) {
        return new LocalCodeManager(properties.getCodeTimeout());
    }

    @Bean
    @ConditionalOnMissingBean(AbstractTokenManager.class)
    public AbstractTokenManager tokenManager(ServerProperties properties) {
        return new LocalTokenManager(properties.getAccessTokenTimeout(), properties.getTimeout());
    }

    @Bean
    @ConditionalOnMissingBean(AbstractTicketGrantingTicketManager.class)
    public AbstractTicketGrantingTicketManager ticketGrantingTicketManager(ServerProperties properties, AbstractTokenManager tokenManager) {
        return new LocalTicketGrantingTicketManager(properties.getTimeout(), properties.getCookieName(), tokenManager);
    }
}