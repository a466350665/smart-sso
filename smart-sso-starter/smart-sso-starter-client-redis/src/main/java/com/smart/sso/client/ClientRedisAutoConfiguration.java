package com.smart.sso.client;

import com.smart.sso.client.listener.LogoutListener;
import com.smart.sso.client.session.SessionMappingStorage;
import com.smart.sso.client.session.redis.RedisSessionMappingStorage;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.session.events.AbstractSessionEvent;
import org.springframework.session.web.http.SessionEventHttpSessionListenerAdapter;

import javax.servlet.http.HttpSessionListener;
import java.util.ArrayList;
import java.util.List;

@Configuration
@AutoConfigureBefore({ ClientAutoConfiguration.class })
public class ClientRedisAutoConfiguration {

	@Bean
	@ConditionalOnMissingBean(SessionMappingStorage.class)
	public SessionMappingStorage sessionMappingStorage() {
		return new RedisSessionMappingStorage();
	}

	/**
	 * 单实例方式单点登出Listener
	 *
	 * @return
	 */
	@Bean
	@ConditionalOnMissingBean(name = "logoutListener")
	public ApplicationListener<AbstractSessionEvent> logoutListener(SessionMappingStorage sessionMappingStorage) {
		List<HttpSessionListener> httpSessionListeners = new ArrayList<>();
		LogoutListener logoutListener = new LogoutListener(sessionMappingStorage);
		httpSessionListeners.add(logoutListener);
		return new SessionEventHttpSessionListenerAdapter(httpSessionListeners);
	}
}