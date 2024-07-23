package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.TokenPermission;

/**
 * 用户信息管理接口
 *
 * @author Joe
 */
public interface PermissionManager {

    /**
     * 获取用户权限信息
     *
     * @param userId
     * @param appId
     * @return
     */
    TokenPermission getUserPermission(Long userId, Long appId);
}
