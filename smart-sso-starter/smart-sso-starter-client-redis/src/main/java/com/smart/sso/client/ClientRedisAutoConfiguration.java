package com.smart.sso.client;

import com.smart.sso.client.token.TokenStorage;
import com.smart.sso.client.token.redis.RedisTokenStorage;
import com.smart.sso.client.util.TokenUtils;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;

@Configuration
@AutoConfigureBefore({ClientAutoConfiguration.class})
public class ClientRedisAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(TokenStorage.class)
    public TokenStorage tokenStorage(ClientProperties properties, StringRedisTemplate redisTemplate) {
        TokenStorage tokenStorage = new RedisTokenStorage(redisTemplate);
        TokenUtils.setTokenStorage(properties, tokenStorage);
        return tokenStorage;
    }
}