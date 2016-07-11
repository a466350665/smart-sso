package com.smart.sso.server.common;

import java.util.UUID;

/**
 * 生成唯一标识
 * 
 * @author Joe
 */
public class KeyGenerator {

	private KeyGenerator() {
	}

	/**
	 * 生成标识,由32位16进制字符组成，字母小写
	 */
	public static String generate() {
		return UUID.randomUUID().toString().replace("-", "").toLowerCase();
	}
}
