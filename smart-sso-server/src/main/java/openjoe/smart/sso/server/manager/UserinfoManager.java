package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Userinfo;

/**
 * 用户信息管理接口
 *
 * @author Joe
 */
public interface UserinfoManager {

    /**
     * 登录
     *
     * @param username 登录名
     * @param password 密码
     * @return
     */
    Result<Userinfo> login(String username, String password);
}
