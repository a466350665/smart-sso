package com.smart.sso.client.token;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.LifecycleManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Token管理
 * 
 * @author Joe
 */
public abstract class TokenStorage implements LifecycleManager<TokenWrapper> {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * 移除
     *
     * @param accessToken
     */
    public abstract void removeByAccessToken(String accessToken);
}
