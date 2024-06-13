package com.smart.sso.client.token;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.LifecycleManager;

/**
 * Token管理
 *
 * @author Joe
 */
public interface TokenStorage extends LifecycleManager<TokenWrapper> {

    /**
     * 创建授权码
     *
     * @param at
     * @return
     */
    default void create(AccessToken at) {
        create(at.getAccessToken(), new TokenWrapper(at, at.getExpiresIn(), at.getRefreshExpiresIn()));
    }
}
