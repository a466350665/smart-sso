package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.Result;
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
     * @param appKey
     * @return
     */
    Result<TokenUser> login(String username, String password, String appKey);
}
