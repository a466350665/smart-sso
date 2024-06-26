package com.smart.sso.client.token;

import com.smart.sso.base.entity.Token;
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
     * @param token
     * @return
     */
    default void create(Token token) {
        create(token.getAccessToken(), new TokenWrapper(token, token.getExpiresIn(), token.getRefreshExpiresIn()));
    }
}
