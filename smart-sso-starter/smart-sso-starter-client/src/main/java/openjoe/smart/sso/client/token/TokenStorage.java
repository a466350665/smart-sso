package openjoe.smart.sso.client.token;

import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.entity.LifecycleManager;
import openjoe.smart.sso.base.entity.TokenPermission;

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
    default void create(Token token, TokenPermission tokenPermission) {
        create(token.getAccessToken(), new TokenWrapper(token, tokenPermission, token.getExpiresIn(), token.getRefreshExpiresIn()));
    }
}
