package com.smart.sso.server.session;

import java.util.UUID;

import com.smart.sso.server.common.AccessTokenContent;
import com.smart.sso.server.common.Expiration;

/**
 * 调用凭证AccessToken管理抽象
 * 
 * @author Joe
 */
public interface AccessTokenManager extends Expiration {

	/**
	 * 生成AccessToken
	 * 
	 * @param service
	 * @param tgt
	 * @return
	 */
	default String generate(String service, String tgt) {
		String resfreshToken = "AT-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(resfreshToken, service, tgt);
		return resfreshToken;
	}

	/**
	 * 生成AccessToken
	 * 
	 * @param accessToken
	 * @param service
	 * @param tgt
	 */
	void create(String accessToken, String service,  String tgt);
	
	/**
	 * 验证accessToken有效性，无论有效性与否，都remove掉
	 * 
	 * @param accessToken
	 * @return
	 */
	AccessTokenContent validate(String accessToken);

	/**
     * 延长AccessToken生命周期
     * 
	 * @param accessToken
	 * @return
	 */
	boolean refresh(String accessToken);
	
	/**
	 * 根据TGT删除AccessToken
	 * 
	 * @param tgt
	 */
	void remove(String tgt);
}
