package com.smart.sso.server;

import com.smart.sso.server.session.AccessTokenManager;
import com.smart.sso.server.session.CodeManager;
import com.smart.sso.server.session.RefreshTokenManager;
import com.smart.sso.server.session.TicketGrantingTicketManager;
import com.smart.sso.server.session.redis.RedisAccessTokenManager;
import com.smart.sso.server.session.redis.RedisCodeManager;
import com.smart.sso.server.session.redis.RedisRefreshTokenManager;
import com.smart.sso.server.session.redis.RedisTicketGrantingTicketManager;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;

@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties({ServerProperties.class})
@AutoConfigureBefore({ ServerAutoConfiguration.class })
public class ServerRedisAutoConfiguration {

	@Bean
	@ConditionalOnMissingBean(AccessTokenManager.class)
	public AccessTokenManager accessTokenManager(StringRedisTemplate redisTemplate, ServerProperties properties) {
		return new RedisAccessTokenManager(redisTemplate, properties.getTimeout());
	}

	@Bean
	@ConditionalOnMissingBean(CodeManager.class)
	public CodeManager codeManager(StringRedisTemplate redisTemplate) {
		return new RedisCodeManager(redisTemplate);
	}

	@Bean
	@ConditionalOnMissingBean(RefreshTokenManager.class)
	public RefreshTokenManager refreshTokenManager(StringRedisTemplate redisTemplate, ServerProperties properties) {
		return new RedisRefreshTokenManager(redisTemplate, properties.getTimeout());
	}

	@Bean
	@ConditionalOnMissingBean(TicketGrantingTicketManager.class)
	public TicketGrantingTicketManager ticketGrantingTicketManager(StringRedisTemplate redisTemplate, ServerProperties properties) {
		return new RedisTicketGrantingTicketManager(redisTemplate, properties.getTimeout());
	}
}