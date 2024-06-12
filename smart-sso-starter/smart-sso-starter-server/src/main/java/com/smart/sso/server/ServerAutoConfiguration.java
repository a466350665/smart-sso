package com.smart.sso.server;

import com.smart.sso.base.BaseAutoConfiguration;
import com.smart.sso.server.token.CodeManager;
import com.smart.sso.server.token.TicketGrantingTicketManager;
import com.smart.sso.server.token.TokenManager;
import com.smart.sso.server.token.local.LocalCodeManager;
import com.smart.sso.server.token.local.LocalTicketGrantingTicketManager;
import com.smart.sso.server.token.local.LocalTokenManager;
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
	@ConditionalOnMissingBean(CodeManager.class)
	public CodeManager codeManager() {
		return new LocalCodeManager();
	}

	@Bean
	@ConditionalOnMissingBean(TokenManager.class)
	public TokenManager tokenManager(ServerProperties properties) {
		return new LocalTokenManager(properties.getTimeout());
	}

	@Bean
	@ConditionalOnMissingBean(TicketGrantingTicketManager.class)
	public TicketGrantingTicketManager ticketGrantingTicketManager(TokenManager tokenManager, ServerProperties properties) {
		return new LocalTicketGrantingTicketManager(tokenManager, properties.getTimeout());
	}
}