package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.base.entity.LifecycleManager;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;
import openjoe.smart.sso.base.util.HttpUtils;
import openjoe.smart.sso.server.entity.CodeContent;
import openjoe.smart.sso.server.entity.TokenContent;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * 调用凭证AccessToken管理抽象
 *
 * @author Joe
 */
public abstract class AbstractTokenManager implements LifecycleManager<TokenContent> {

    /**
     * accessToken超时时效
     */
    private int accessTokenTimeout;

    /**
     * refreshToken时效和登录超时时效保持一致
     */
    private int refreshTokenTimeout;

    public AbstractTokenManager(int accessTokenTimeout, int refreshTokenTimeout) {
        this.accessTokenTimeout = accessTokenTimeout;
        this.refreshTokenTimeout = refreshTokenTimeout;
    }

    /**
     * 通过TGT移除
     *
     * @param tgt
     */
    public abstract void removeByTgt(String tgt);

    /**
     * 创建AccessToken
     *
     * @param tc
     * @return
     */
    public TokenContent create(TokenContent tc) {
        return create(tc.getTokenUser(), tc.getTokenPermission(), tc.getClientId(), tc);
    }

    /**
     * 创建AccessToken
     *
     * @param tokenUser
     * @param tokenPermission
     * @param clientId
     * @param codeContent
     * @return
     */
    public TokenContent create(TokenUser tokenUser, TokenPermission tokenPermission, String clientId, CodeContent codeContent) {
        String accessToken = "AT-" + UUID.randomUUID().toString().replaceAll("-", "");
        String refreshToken = "RT-" + UUID.randomUUID().toString().replaceAll("-", "");
        TokenContent tc = new TokenContent(accessToken, refreshToken, tokenUser, tokenPermission, clientId, codeContent.getTgt(), codeContent.getRedirectUri());
        create(refreshToken, tc);
        return tc;
    }

    /**
     * 发起客户端退出请求
     *
     * @param redirectUri
     * @param accessToken
     */
    protected void sendLogoutRequest(String redirectUri, String accessToken) {
        Map<String, String> headerMap = new HashMap<>();
        headerMap.put(BaseConstant.LOGOUT_PARAMETER_NAME, accessToken);
        HttpUtils.postHeader(redirectUri, headerMap);
    }

    public int getAccessTokenTimeout() {
        return accessTokenTimeout;
    }

    public void setAccessTokenTimeout(int accessTokenTimeout) {
        this.accessTokenTimeout = accessTokenTimeout;
    }

    public int getRefreshTokenTimeout() {
        return refreshTokenTimeout;
    }

    public void setRefreshTokenTimeout(int refreshTokenTimeout) {
        this.refreshTokenTimeout = refreshTokenTimeout;
    }
}
