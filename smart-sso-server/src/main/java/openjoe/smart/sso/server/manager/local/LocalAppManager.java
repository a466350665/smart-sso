package openjoe.smart.sso.server.manager.local;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.server.manager.AppManager;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * 应用服务
 */
@Component
public class LocalAppManager implements AppManager {

    private static List<App> appList;

    static {
        appList = new ArrayList<>();
        appList.add(new App("客户端1", "demo1", "123456"));
        appList.add(new App("客户端2", "demo2", "123456"));
    }

    @Override
    public boolean exists(String appId) {
        return appList.stream().anyMatch(app -> app.getAppId().equals(appId));
    }

    @Override
    public Result<Void> validate(String appId, String appSecret) {
        for (App app : appList) {
            if (app.getAppId().equals(appId)) {
                if (app.getAppSecret().equals(appSecret)) {
                    return Result.success();
                } else {
                    return Result.createError("appSecret有误");
                }
            }
        }
        return Result.createError("appId不存在");
    }

    /**
     * 应用
     */
    static class App {

        /**
         * 名称
         */
        private String name;
        /**
         * 应用唯一标识
         */
        private String appId;
        /**
         * 应用密钥
         */
        private String appSecret;

        public App() {
            super();
        }

        public App(String name, String appId, String appSecret) {
            super();
            this.name = name;
            this.appId = appId;
            this.appSecret = appSecret;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getAppId() {
            return appId;
        }

        public void setAppId(String appId) {
            this.appId = appId;
        }

        public String getAppSecret() {
            return appSecret;
        }

        public void setAppSecret(String appSecret) {
            this.appSecret = appSecret;
        }
    }
}
