package openjoe.smart.sso.server.service;

import openjoe.smart.sso.base.entity.Result;

/**
 * 应用服务接口
 *
 * @author Joe
 */
public interface AppService {

    /**
     * 应用是否存在
     * @param appId
     * @return
     */
    boolean exists(String appId);

    /**
     * 校验应用的登记信息
     * @param appId
     * @param appSecret
     * @return
     */
    Result<Void> validate(String appId, String appSecret);
}
