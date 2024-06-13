package com.smart.sso.base.entity;

/**
 * 时效清理接口
 *
 * @author Joe
 */
public interface ExpirationPolicy {

    /**
     * 定时清理时效过期的凭证
     */
    void verifyExpired();
}
