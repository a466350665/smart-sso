package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;

/**
 * 用户信息管理接口
 *
 * @author Joe
 */
public interface UserManager {

    /**
     * 登录
     *
     * @param username 登录名
     * @param password 密码
     * @return
     */
    Result<TokenUser> login(String username, String password);

    /**
     * 获取用户权限信息
     *
     * @param userId
     * @param clientId
     * @return
     */
    TokenPermission getUserPermission(Long userId, String clientId);
}
