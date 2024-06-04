package com.smart.sso.server.session;

import com.smart.sso.server.common.Expiration;
import com.smart.sso.server.common.ServerUser;

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
	default String generate(ServerUser user) {
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
    void create(String tgt, ServerUser user);
    
    /**
     * 验证st是否存在且在有效期内，并更新过期时间戳
     * 
     * @param tgt
     * @return
     */
    ServerUser getAndRefresh(String tgt);
    
    /**
     * 设置新的用户信息
     * 
     * @param user
     * @return
     */
    void set(String tgt, ServerUser user);
    
    /**
     * 移除
     * 
     * @param tgt
     */
    void remove(String tgt);
}
