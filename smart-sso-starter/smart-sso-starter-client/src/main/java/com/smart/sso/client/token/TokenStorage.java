package com.smart.sso.client.token;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.base.entity.LifecycleManager;
import com.smart.sso.client.ClientProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Token管理
 * 
 * @author Joe
 */
public abstract class TokenStorage implements LifecycleManager<AccessToken> {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * 验证accessToken是否存在且在有效期内，过期使用refresh接口刷新
     *
     * @param st
     * @return
     */
    public abstract AccessToken getAndRefresh(String st);

    /**
     * 移除
     *
     * @param accessToken
     */
    public abstract void removeByAccessToken(String accessToken);
}
