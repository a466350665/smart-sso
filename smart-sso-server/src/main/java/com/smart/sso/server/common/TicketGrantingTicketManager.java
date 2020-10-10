package com.smart.sso.server.common;

import com.smart.sso.client.dto.RpcUserDto;

/**
 * 令牌（TGT）管理抽象
 * 
 * @author Joe
 */
public abstract class TicketGrantingTicketManager extends TicketManager {

    public TicketGrantingTicketManager() {
        super();
        // TGT默认超时时间2小时
        setTimeout(7200);
    }

    /**
     * 登录成功后，根据用户信息生成令牌
     * 
     * @param user
     * @return
     */
    public abstract String generate(RpcUserDto user);
    
    /**
     * 验证TGT是否存在，且在有效期内
     * 
     * @param tgt
     * @return
     */
    public abstract RpcUserDto validate(String tgt);
    
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
     * @param service
     * @return
     */
    public abstract String signSt(String tgt, String service);
}
