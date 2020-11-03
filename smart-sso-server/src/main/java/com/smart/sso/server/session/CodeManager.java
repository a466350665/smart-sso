package com.smart.sso.server.session;

import java.util.UUID;

import com.smart.sso.server.common.CodeContent;
import com.smart.sso.server.common.Expiration;

/**
 * 授权码code管理
 * 
 * @author Joe
 */
public interface CodeManager extends Expiration {
	
	/**
	 * 生成授权码
	 * 
	 * @param service
	 * @param tgt
	 * @return
	 */
	default String generate(String service, String tgt) {
		String st = "code-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(st, service, tgt);
		return st;
	}
    
    /**
     * 生成授权码
     * 
	 * @param code
	 * @param service
	 * @param tgt
	 */
	public void create(String code, String service, String tgt) ;

    /**
     * 验证授权码有效性，无论有效性与否，都remove掉
     * 
     * @param code
     * @return
     */
	CodeContent validate(String code);
	
	/* 
	 * code失效时间默认为10分钟
	 */
	@Override
	default int getExpiresIn() {
		return 600;
	}
}
