package com.smart.sso.server;

import com.smart.sso.server.token.AbstractCodeManager;
import com.smart.sso.server.token.AbstractTicketGrantingTicketManager;
import com.smart.sso.server.token.AbstractTokenManager;
import com.smart.sso.server.token.redis.RedisCodeManager;
import com.smart.sso.server.token.redis.RedisTicketGrantingTicketManager;
import com.smart.sso.server.token.redis.RedisTokenManager;
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
        return new RedisTokenManager(properties.getAccessTokenTimeout(), properties.getTimeout(), redisTemplate);
    }

    @Bean
    @ConditionalOnMissingBean(AbstractTicketGrantingTicketManager.class)
    public AbstractTicketGrantingTicketManager ticketGrantingTicketManager(ServerProperties properties, AbstractTokenManager tokenManager, StringRedisTemplate redisTemplate) {
        return new RedisTicketGrantingTicketManager(properties.getTimeout(), properties.getCookieName(), tokenManager, redisTemplate);
    }
}