package com.smart.sso.server.common;

/**
 * 票据（ST）管理抽象
 * 
 * @author Joe
 */
public abstract class ServiceTicketManager extends TicketManager {
    
    public ServiceTicketManager() {
        super();
        // ST默认超时时间10秒
        setTimeout(10);
    }

    /**
     * 生成票据
     * 
     * @param tgt
     */
    public abstract String generate(String tgt);

    /**
     * 验证票据有效性，无论有效性与否，都remove掉ticket
     * 
     * @param ticket
     * @return
     */
    public abstract String validate(String ticket);

    /**
     * 移除票据
     * 
     * @param ticket
     */
    public abstract void remove(String ticket);
}
