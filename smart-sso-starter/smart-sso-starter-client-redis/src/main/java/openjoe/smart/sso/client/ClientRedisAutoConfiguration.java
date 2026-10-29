package openjoe.smart.sso.client;

import openjoe.smart.sso.client.token.TokenPermissionStorage;
import openjoe.smart.sso.client.token.SsoTokenStorage;
import openjoe.smart.sso.client.token.redis.RedisTokenPermissionStorage;
import openjoe.smart.sso.client.token.redis.RedisTokenStorage;
import openjoe.smart.sso.client.util.SsoUtils;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;

@Configuration
@AutoConfigureBefore({ClientAutoConfiguration.class})
public class ClientRedisAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(SsoTokenStorage.class)
    public SsoTokenStorage ssoTokenStorage(ClientProperties properties, StringRedisTemplate redisTemplate) {
        SsoTokenStorage tokenStorage = new RedisTokenStorage(redisTemplate);
        TokenPermissionStorage tokenPermissionStorage = new RedisTokenPermissionStorage(redisTemplate);
        SsoUtils.setTokenStorage(properties, tokenStorage, tokenPermissionStorage);
        return tokenStorage;
    }
}
