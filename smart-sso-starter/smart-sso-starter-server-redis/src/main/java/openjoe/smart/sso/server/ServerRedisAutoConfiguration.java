package openjoe.smart.sso.server;

import openjoe.smart.sso.server.manager.AbstractCodeManager;
import openjoe.smart.sso.server.manager.AbstractTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.AbstractTokenManager;
import openjoe.smart.sso.server.manager.redis.RedisCodeManager;
import openjoe.smart.sso.server.manager.redis.RedisTicketGrantingTicketManager;
import openjoe.smart.sso.server.manager.redis.RedisTokenManager;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;

@Configuration(proxyBeanMethods = false)
@AutoConfigureBefore({ServerAutoConfiguration.class})
@EnableConfigurationProperties({ServerProperties.class})
public class ServerRedisAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(AbstractCodeManager.class)
    public AbstractCodeManager codeManager(ServerProperties properties, StringRedisTemplate redisTemplate) {
        return new RedisCodeManager(properties.getCodeTimeout(), redisTemplate);
    }

    @Bean
    @ConditionalOnMissingBean(AbstractTokenManager.class)
    public AbstractTokenManager tokenManager(ServerProperties properties, StringRedisTemplate redisTemplate) {
        return new RedisTokenManager(properties.getAccessTokenTimeout(), properties.getTimeout(), properties.getThreadPoolSize(), redisTemplate);
    }

    @Bean
    @ConditionalOnMissingBean(AbstractTicketGrantingTicketManager.class)
    public AbstractTicketGrantingTicketManager ticketGrantingTicketManager(ServerProperties properties, AbstractTokenManager tokenManager, StringRedisTemplate redisTemplate) {
        return new RedisTicketGrantingTicketManager(properties.getTimeout(), properties.getCookieName(), tokenManager, redisTemplate);
    }
}