package com.smart.sso.server;

import com.smart.sso.base.BaseAutoConfiguration;
import com.smart.sso.server.token.*;
import com.smart.sso.server.token.local.LocalAccessTokenManager;
import com.smart.sso.server.token.local.LocalCodeManager;
import com.smart.sso.server.token.local.LocalRefreshTokenManager;
import com.smart.sso.server.token.local.LocalTicketGrantingTicketManager;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration(proxyBeanMethods = false)
@AutoConfigureBefore({ BaseAutoConfiguration.class })
@EnableConfigurationProperties({ServerProperties.class})
public class ServerAutoConfiguration {

	@Bean
	@ConditionalOnMissingBean(AccessTokenManager.class)
	public AccessTokenManager accessTokenManager(ServerProperties properties) {
		return new LocalAccessTokenManager(properties.getTimeout());
	}

	@Bean
	@ConditionalOnMissingBean(CodeManager.class)
	public CodeManager codeManager() {
		return new LocalCodeManager();
	}

	@Bean
	@ConditionalOnMissingBean(RefreshTokenManager.class)
	public RefreshTokenManager refreshTokenManager(ServerProperties properties) {
		return new LocalRefreshTokenManager(properties.getTimeout());
	}

	@Bean
	@ConditionalOnMissingBean(TicketGrantingTicketManager.class)
	public TicketGrantingTicketManager ticketGrantingTicketManager(ServerProperties properties) {
		return new LocalTicketGrantingTicketManager(properties.getTimeout());
	}

	@Bean
	@ConditionalOnMissingBean(TokenManager.class)
	public TokenManager tManager(AccessTokenManager accessTokenManager, TicketGrantingTicketManager ticketGrantingTicketManager) {
		return new TokenManager(accessTokenManager, ticketGrantingTicketManager);
	}
}