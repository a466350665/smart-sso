package com.smart.sso.server.session;

import java.util.UUID;

import com.smart.sso.client.rpc.RpcUser;

/**
 * 登录凭证（TGT）管理抽象
 * 
 * @author Joe
 */
public interface TicketGrantingTicketManager {
	
    /**
     * 登录成功后，根据用户信息生成令牌
     * 
     * @param user
     * @return
     */
	default String generate(RpcUser user) {
		String tgt = "TGT-" + UUID.randomUUID().toString().replaceAll("-", "");
		generate(tgt, user);
		return tgt;
	}
    
    /**
     * 登录成功后，根据用户信息生成令牌
     * 
     * @param user
     * @return
     */
    void generate(String tgt, RpcUser user);
    
    /**
     * 验证st是否存在，且在有效期内
     * 
     * @param tgt
     * @return
     */
    RpcUser exists(String tgt);
    
    /**
     * 移除
     * 
     * @param tgt
     */
    void remove(String tgt);
    
    /**
     * 存在，则延长TGT生命周期，返回true。不存在，返回false
     * 
     * @param tgt
     * @return
     */
    boolean refresh(String tgt);
}
