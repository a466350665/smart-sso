package openjoe.smart.sso.client.token;

import openjoe.smart.sso.base.entity.ExpirationWrapper;
import openjoe.smart.sso.base.entity.LifecycleManager;
import openjoe.smart.sso.base.entity.Token;
import openjoe.smart.sso.base.entity.TokenPermission;

/**
 * 凭证权限信息管理
 *
 * @author Joe
 */
public interface TokenPermissionStorage extends LifecycleManager<ExpirationWrapper<TokenPermission>> {

    /**
     * 创建凭证权限信息
     *
     * @param token
     * @return
     */
    default void create(Token token, TokenPermission tokenPermission) {
        create(token.getAccessToken(), new ExpirationWrapper<>(tokenPermission, token.getRefreshExpiresIn()));
    }
}
