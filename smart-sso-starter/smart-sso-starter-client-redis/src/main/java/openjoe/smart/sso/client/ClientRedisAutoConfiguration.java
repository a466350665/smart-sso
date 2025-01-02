package openjoe.smart.sso.client;

import openjoe.smart.sso.client.token.TokenPermissionStorage;
import openjoe.smart.sso.client.token.TokenStorage;
import openjoe.smart.sso.client.token.redis.RedisTokenPermissionStorage;
import openjoe.smart.sso.client.token.redis.RedisTokenStorage;
import openjoe.smart.sso.client.util.SSOUtils;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;

@Configuration
@AutoConfigureAfter({ClientAutoConfiguration.class})
public class ClientRedisAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(TokenStorage.class)
    public TokenStorage tokenStorage(ClientProperties properties, StringRedisTemplate redisTemplate) {
        TokenStorage tokenStorage = new RedisTokenStorage(redisTemplate);
        TokenPermissionStorage tokenPermissionStorage = new RedisTokenPermissionStorage(redisTemplate);
        SSOUtils.setTokenStorage(properties, tokenStorage, tokenPermissionStorage);
        return tokenStorage;
    }
}