package com.smart.mvc.cache;

import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;

import com.smart.mvc.exception.CacheException;

public class RedisCache<K, V> {

	private static final Logger LOGGER = LoggerFactory.getLogger(RedisCache.class);

	private RedisTemplate<K, V> redisTemplate;

	public V get(K key) {
		LOGGER.debug("get key [" + key + "]");
		try {
			if (key == null) {
				return null;
			}
			else {
				return redisTemplate.opsForValue().get(key);
			}
		}
		catch (Throwable t) {
			throw new CacheException(t);
		}

	}

	public V set(K key, V value) {
		LOGGER.debug("set key [" + key + "]");
		try {
			redisTemplate.opsForValue().set(key, value);
			return value;
		}
		catch (Throwable t) {
			throw new CacheException(t);
		}
	}

	public V set(K key, V value, long timeout) {
		LOGGER.debug("set key [" + key + "]");
		try {
			redisTemplate.opsForValue().set(key, value, timeout, TimeUnit.MINUTES);
			return value;
		}
		catch (Throwable t) {
			throw new CacheException(t);
		}
	}

	public void delete(K key) {
		LOGGER.debug("delete key [" + key + "]");
		try {
			redisTemplate.delete(key);
		}
		catch (Throwable t) {
			throw new CacheException(t);
		}
	}

	public void setRedisTemplate(RedisTemplate<K, V> redisTemplate) {
		this.redisTemplate = redisTemplate;
	}
}