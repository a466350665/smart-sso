package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.Result;

/**
 * 应用管理接口
 *
 * @author Joe
 */
public interface AppManager {

    /**
     * 应用是否存在
     * @param appKey
     * @return
     */
    boolean exists(String appKey);

    /**
     * 校验应用的登记信息
     * @param appKey
     * @param appSecret
     * @return
     */
    Result<Void> validate(String appKey, String appSecret);
}
