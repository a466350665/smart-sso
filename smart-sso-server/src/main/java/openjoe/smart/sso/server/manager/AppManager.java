package openjoe.smart.sso.server.manager;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.server.entity.App;

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
    Result<Void> validate(String clientId);

    /**
     * 校验应用的登记信息
     * @param clientId
     * @param clientSecret
     * @return
     */
    Result<Void> validate(String clientId, String clientSecret);
}
