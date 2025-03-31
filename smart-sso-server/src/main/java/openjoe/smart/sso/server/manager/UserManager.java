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
     * 校验
     *
     * @param username 登录名
     * @param password 密码
     * @return
     */
    Result<Long> validate(String username, String password);

    /**
     * 获取Token需要的用户信息
     *
     * @param userId 用户ID
     * @return
     */
    Result<TokenUser> getTokenUser(Long userId);
}
