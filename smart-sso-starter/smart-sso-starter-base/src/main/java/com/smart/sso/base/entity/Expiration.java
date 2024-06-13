package com.smart.sso.base.entity;

/**
 * 时效接口
 *
 * @author Joe
 */
public interface Expiration {

    /**
     * 获取时效时间（秒）
     *
     * @return
     */
    int getExpiresIn();
}
