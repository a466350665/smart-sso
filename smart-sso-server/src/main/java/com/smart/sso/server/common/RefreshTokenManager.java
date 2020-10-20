package com.smart.sso.server.common;

/**
 * refreshToken管理抽象
 * 
 * @author Joe
 */
public abstract class RefreshTokenManager extends TimeoutManager {
    
    public RefreshTokenManager(int timeout) {
        super(timeout);
    }

    /**
     * 生成refreshToken
     * 
     * @param tgt
     * @return
     */
    public abstract String generate(String tgt);
    
    /**
     * 验证refreshToken有效性，无论有效性与否，都remove掉
     * 
     * @param rt
     * @return
     */
    public abstract String validate(String rt);
}
