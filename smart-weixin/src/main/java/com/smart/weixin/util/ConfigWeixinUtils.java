package com.smart.weixin.util;

import com.smart.ssm.util.ConfigManager;
import com.smart.ssm.util.EhCacheConfig;

/**
 * 应用配置工具类
 */
public class ConfigWeixinUtils {

	public static final String DEFAULT_NAMESPACE = "weixin";

	public static final String CONFIG_FILE_NAME = "config-weixin.xml";

	public static String getValue(String key, String namespace) {
		return ConfigManager.getValue(key, namespace, CONFIG_FILE_NAME, DEFAULT_NAMESPACE,
				EhCacheConfig.getInstance());
	}

	public static String getValue(String key) {
		return getValue(key, DEFAULT_NAMESPACE);
	}
}