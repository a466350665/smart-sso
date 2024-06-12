package com.smart.sso.base.entity;

/**
 * 生命周期管理器
 *
 * @author Joe
 */
public interface LifecycleManager<T> {

    /**
     * 创建
     *
     * @param key
     * @param value
     */
    void create(String key, T value);

    /**
     * 获取
     *
     * @param key
     * @return
     */
    T get(String key);

    /**
     * 移除
     *
     * @param key
     */
    void remove(String key);
}
