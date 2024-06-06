package com.smart.sso.base.entity;

/**
 * 含时效
 * 
 * @author Joe
 */
public interface Expiration {
	
	/**
	 * 时效（秒）
	 * @return
	 */
	int getExpiresIn();
}
