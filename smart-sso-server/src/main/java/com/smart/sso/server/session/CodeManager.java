package com.smart.sso.server.session;

import java.util.UUID;

import com.smart.sso.server.common.AuthContent;
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
	 * @param tgt
	 * @param clientType
	 * @param redirectUri
	 * @return
	 */
	default String generate(String tgt, boolean sendLogoutRequest, String redirectUri) {
		String code = "code-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(code, new AuthContent(tgt, sendLogoutRequest, redirectUri));
		return code;
	}
    
    /**
     * 生成授权码
     * 
	 * @param code
	 * @param authContent
	 */
	public void create(String code, AuthContent authContent) ;

    /**
     * 验证授权码有效性，无论有效性与否，都remove掉
     * 
     * @param code
     * @return
     */
	AuthContent validate(String code);
	
	/* 
	 * code失效时间默认为10分钟
	 */
	@Override
	default int getExpiresIn() {
		return 600;
	}
}
