package com.smart.ssm.util;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 工具类 - 缓存
 * 
 * @author Joe
 */
public class EhCacheConfig implements CacheConfig {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(EhCacheConfig.class);
	
	public static final String GENERAL_CACHE_ADMINISTRATOR_BEAN_NAME = "cacheManager";

	private static EhCacheConfig config;

	private static CacheManager manage;
	
	static{
		try {
			manage = SpringUtils.getBean(GENERAL_CACHE_ADMINISTRATOR_BEAN_NAME);
			config = new EhCacheConfig();
		}
		catch (Exception e) {
			LOGGER.error("create cache instance exception", e);
		}
	}

	private EhCacheConfig() {
	}

	public static EhCacheConfig getInstance() {
		return config;
	}

	/**
	 * 根据Key读取缓存
	 * 
	 * @return 缓存对象
	 */
	public Object getCache(String cacheName, String key) {
		Element e = manage.getCache(cacheName).get(key);
		if (e == null)
			return null;
		return e.getObjectValue();
	}

	/**
	 * 加入或刷新对象到缓存
	 * 
	 */
	public void putCache(String cacheName, String key, Object object) {
		manage.getCache(cacheName).put(new Element(key, object));
	}
}