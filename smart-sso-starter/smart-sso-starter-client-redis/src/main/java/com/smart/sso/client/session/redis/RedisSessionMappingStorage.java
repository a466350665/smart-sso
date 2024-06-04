package com.smart.sso.client.session.redis;

import com.smart.sso.client.session.SessionMappingStorage;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.session.SessionRepository;

import javax.servlet.http.HttpSession;

/**
 * 借鉴CAS
 * 
 * @author Joe
 */
public final class RedisSessionMappingStorage implements SessionMappingStorage {
	
	private static final String SESSION_TOKEN_KEY = "session_token_key_";
	private static final String TOKEN_SESSION_KEY = "token_session_key_";

	private StringRedisTemplate redisTemplate;
    private SessionRepository<?> sessionRepository;

    public RedisSessionMappingStorage(StringRedisTemplate redisTemplate, SessionRepository<?> sessionRepository) {
        this.redisTemplate = redisTemplate;
        this.sessionRepository = sessionRepository;
    }

    @Override
    public synchronized void addSessionById(final String accessToken, final HttpSession session) {
        redisTemplate.opsForValue().set(SESSION_TOKEN_KEY + session.getId(), accessToken);
		
		redisTemplate.opsForValue().set(TOKEN_SESSION_KEY + accessToken, session.getId());
    }

    @Override
    public synchronized void removeBySessionById(final String sessionId) {
		final String accessToken = redisTemplate.opsForValue().get(SESSION_TOKEN_KEY + sessionId);
		if (accessToken != null) {
			redisTemplate.delete(TOKEN_SESSION_KEY + accessToken);
			redisTemplate.delete(SESSION_TOKEN_KEY + sessionId);
			
			sessionRepository.deleteById(sessionId);
		}
    }

    @Override
    public synchronized HttpSession removeSessionByMappingId(final String accessToken) {
        final String sessionId = redisTemplate.opsForValue().get(TOKEN_SESSION_KEY + accessToken);
        if (sessionId != null) {
            removeBySessionById(sessionId);
        }
        return null;
    }
}
