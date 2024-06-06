package com.smart.sso.server.token;

import com.smart.sso.base.entity.Expiration;
import com.smart.sso.server.entity.CodeContent;

import java.util.UUID;

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
	 * @param sendLogoutRequest
	 * @param redirectUri
	 * @return
	 */
	default String generate(String tgt, boolean sendLogoutRequest, String redirectUri) {
		String code = "code-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(code, new CodeContent(tgt, sendLogoutRequest, redirectUri));
		return code;
	}
    
    /**
     * 生成授权码
     * 
	 * @param code
	 * @param codeContent
	 */
	public void create(String code, CodeContent codeContent) ;

    /**
     * 查找并删除
     * 
     * @param code
     * @return
     */
	CodeContent getAndRemove(String code);
	
	/* 
	 * code失效时间默认为10分钟
	 */
	@Override
	default int getExpiresIn() {
		return 600;
	}
}
