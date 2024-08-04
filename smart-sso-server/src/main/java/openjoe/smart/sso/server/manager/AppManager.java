package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.Result;

/**
 * 应用管理接口
 *
 * @author Joe
 */
public interface AppManager {

    /**
     * 校验应用是否存在
     * @param clientId
     * @return
     */
    Result<Long> validate(String clientId);

    /**
     * 校验应用的登记信息
     * @param clientId
     * @param clientSecret
     * @return
     */
    Result<Void> validate(String clientId, String clientSecret);
}
