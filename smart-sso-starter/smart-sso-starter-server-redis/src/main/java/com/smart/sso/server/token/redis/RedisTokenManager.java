package com.smart.sso.server.token.redis;

import com.smart.sso.base.util.JsonUtils;
import com.smart.sso.server.entity.TokenContent;
import com.smart.sso.server.token.TokenManager;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * 分布式调用凭证管理
 * 
 * @author Joe
 */
public class RedisTokenManager extends TokenManager {

	private StringRedisTemplate redisTemplate;

	public RedisTokenManager(int timeout, StringRedisTemplate redisTemplate) {
		super(timeout);
		this.redisTemplate = redisTemplate;
	}

	@Override
	public void create(String refreshToken, TokenContent tokenContent) {
		redisTemplate.opsForValue().set(refreshToken, JsonUtils.toJSONString(tokenContent), getExpiresIn(),
				TimeUnit.SECONDS);

		redisTemplate.opsForSet().add(getKey(tokenContent.getCodeContent().getTgt()), refreshToken);
	}
	
	@Override
	public TokenContent get(String refreshToken) {
		String atcStr = redisTemplate.opsForValue().get(refreshToken);
		if (StringUtils.isEmpty(atcStr)) {
			return null;
		}
		return JsonUtils.parseObject(atcStr, TokenContent.class);
	}

	@Override
	public void remove(String refreshToken) {
		String atcStr = redisTemplate.opsForValue().get(refreshToken);
		if (StringUtils.isEmpty(atcStr)) {
			return;
		}
		redisTemplate.delete(refreshToken);

		// TGT集合中删除当前refreshToken
		TokenContent tokenContent = JsonUtils.parseObject(atcStr, TokenContent.class);
		if (tokenContent == null) {
			return;
		}
		redisTemplate.opsForSet().remove(getKey(tokenContent.getCodeContent().getTgt()), refreshToken);
	}

	@Override
	public void removeByTgt(String tgt) {
		Set<String> accessTokenSet = redisTemplate.opsForSet().members(getKey(tgt));
		if (CollectionUtils.isEmpty(accessTokenSet)) {
			return;
		}
		redisTemplate.delete(getKey(tgt));

		accessTokenSet.forEach(refreshToken -> {
			String atcStr = redisTemplate.opsForValue().get(refreshToken);
			if (StringUtils.isEmpty(atcStr)) {
				return;
			}
			redisTemplate.delete(refreshToken);

			TokenContent tokenContent = JsonUtils.parseObject(atcStr, TokenContent.class);
			if (tokenContent == null) {
				return;
			}
			sendLogoutRequest(tokenContent.getCodeContent().getRedirectUri(), tokenContent.getAccessToken());
		});
	}

	private String getKey(String tgt) {
		return tgt + "_access_token";
	}
}
