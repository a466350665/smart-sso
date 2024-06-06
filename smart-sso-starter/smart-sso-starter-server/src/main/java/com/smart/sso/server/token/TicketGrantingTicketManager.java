package com.smart.sso.server.token;

import com.smart.sso.base.entity.Expiration;
import com.smart.sso.base.entity.Userinfo;

import java.util.UUID;

/**
 * 登录凭证（TGT）管理抽象
 * 
 * @author Joe
 */
public interface TicketGrantingTicketManager extends Expiration {
	
    /**
     * 登录成功后，根据用户信息生成令牌
     * 
     * @param user
     * @return
     */
	default String generate(Userinfo user) {
		String tgt = "TGT-" + UUID.randomUUID().toString().replaceAll("-", "");
		create(tgt, user);
		return tgt;
	}
    
    /**
     * 登录成功后，根据用户信息生成令牌
     * 
     * @param user
     * @return
     */
    void create(String tgt, Userinfo user);
    
    /**
     * 验证st是否存在且在有效期内，并更新过期时间戳
     * 
     * @param tgt
     * @return
     */
    Userinfo getAndRefresh(String tgt);
    
    /**
     * 设置新的用户信息
     * 
     * @param user
     * @return
     */
    void set(String tgt, Userinfo user);
    
    /**
     * 移除
     * 
     * @param tgt
     */
    void remove(String tgt);
}
