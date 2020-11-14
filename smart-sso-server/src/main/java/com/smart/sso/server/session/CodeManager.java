package com.smart.sso.server.session;

import java.util.UUID;

import com.smart.sso.server.common.CodeContent;
import com.smart.sso.server.common.Expiration;
import com.smart.sso.server.enums.ClientTypeEnum;

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
	default String generate(String tgt, ClientTypeEnum clientType, String redirectUri) {
		String code = "code-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(code, new CodeContent(tgt, clientType, redirectUri));
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
