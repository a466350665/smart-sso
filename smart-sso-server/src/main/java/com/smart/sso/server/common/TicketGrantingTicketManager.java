package com.smart.sso.server.common;

import com.smart.sso.client.dto.SsoUser;

/**
 * 令牌（TGT）管理抽象
 * 
 * @author Joe
 */
public abstract class TicketGrantingTicketManager extends TimeoutManager {
    
    public TicketGrantingTicketManager(int timeout) {
        super(timeout);
    }

    /**
     * 登录成功后，根据用户信息生成令牌
     * 
     * @param user
     * @return
     */
    public abstract String generate(SsoUser user);
    
    /**
     * 验证st是否存在，且在有效期内
     * 
     * @param tgt
     * @return
     */
    public abstract SsoUser exists(String tgt);
    
    /**
     * 移除
     * 
     * @param tgt
     */
    public abstract void remove(String tgt);
    
    /**
     * 签发ST
     * 
     * @param tgt
     * @param st
     * @param service
     * @return
     */
    public abstract String signSt(String tgt, String st, String service);
    
    /**
     * 延长TGT生命周期
     * 
     * @param tgt
     * @return
     */
    public abstract boolean refresh(String tgt);
}
