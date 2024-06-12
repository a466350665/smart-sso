package com.smart.sso.server;

import com.smart.sso.server.token.CodeManager;
import com.smart.sso.server.token.TicketGrantingTicketManager;
import com.smart.sso.server.token.TokenManager;
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
@AutoConfigureBefore({ ServerAutoConfiguration.class })
@EnableConfigurationProperties({ServerProperties.class})
public class ServerRedisAutoConfiguration {

	@Bean
	@ConditionalOnMissingBean(CodeManager.class)
	public CodeManager codeManager(StringRedisTemplate redisTemplate) {
		return new RedisCodeManager(redisTemplate);
	}

	@Bean
	@ConditionalOnMissingBean(TokenManager.class)
	public TokenManager tokenManager(ServerProperties properties, StringRedisTemplate redisTemplate) {
		return new RedisTokenManager(properties.getTimeout(), redisTemplate);
	}

	@Bean
	@ConditionalOnMissingBean(TicketGrantingTicketManager.class)
	public TicketGrantingTicketManager ticketGrantingTicketManager(TokenManager tokenManager, ServerProperties properties, StringRedisTemplate redisTemplate) {
		return new RedisTicketGrantingTicketManager(tokenManager, properties.getTimeout(), redisTemplate);
	}
}