package com.smart.sso.server.common;

/**
 * ST管理抽象
 * 
 * @author Joe
 */
public abstract class ServiceTicketManager extends TimeoutManager {
    
    /**
     * 带时效参数构造方法
     * 
     * @param timeout
     */
    public ServiceTicketManager(int timeout) {
        super(timeout);
    }

    /**
     * 生成ST
     * 
     * @param tgt
     * @return
     */
    public abstract String generate(String tgt);

    /**
     * 验证票据有效性，无论有效性与否，都remove掉st
     * 
     * @param st
     * @return
     */
    public abstract String validate(String st);
    
    /**
     * 移除票据
     * 
     * @param st
     */
    public abstract void remove(String st);
}
