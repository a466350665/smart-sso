package com.smart.sso.client.token;

import com.smart.sso.base.entity.AccessToken;
import com.smart.sso.client.ClientProperties;
import com.smart.sso.client.entity.Result;
import com.smart.sso.client.util.Oauth2Utils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Token管理
 * 
 * @author Joe
 */
public abstract class TokenStorage {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    protected ClientProperties properties;

    /**
     * 登录成功后，根据用户信息生成令牌
     *
     * @param st
     * @return
     */
    public abstract void create(String st, AccessToken accessToken);

    /**
     * 验证st是否存在且在有效期内，并更新过期时间戳
     *
     * @param st
     * @return
     */
    public abstract AccessToken getAndRefresh(String st);

    /**
     * 移除
     *
     * @param st
     */
    public abstract void removeByServiceTicket(String st);

    public abstract void removeByAccessToken(String accessToken);

    /**
     * 通过refreshToken参数调用http请求延长服务端Token时效，并返回新的accessToken
     *
     * @param refreshToken
     * @return
     */
    protected AccessToken refreshToken(String refreshToken) {
        Result<AccessToken> result = Oauth2Utils.refreshToken(properties.getServerUrl(), properties.getAppId(), refreshToken);
        if (!result.isSuccess()) {
            logger.error("refreshToken has error, message:{}", result.getMessage());
            return null;
        }
        return result.getData();
    }
}
