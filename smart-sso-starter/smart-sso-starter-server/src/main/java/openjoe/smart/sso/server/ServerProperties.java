package openjoe.smart.sso.server;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties("smart.sso.server")
public class ServerProperties {

    /*
     * 授权码失效时间，默认为10分钟（单位秒）
     */
    private int codeTimeout = 600;

    /*
     * 调用凭证accessToken失效时间，默认为30分钟（单位秒）
     */
    private int accessTokenTimeout = 1800;

    /**
     * 单点登录失效时间，默认2小时（单位秒）
     *
     * 注：刷新凭证refreshToken和登录服务端凭证TGT共用的失效时间
     */
    private int timeout = 7200;

    /**
     * cookie中TGT名称，和CAS概念保持一致
     */
    private String cookieName = "TGC";

    public int getTimeout() {
        return timeout;
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }

    public int getCodeTimeout() {
        return codeTimeout;
    }

    public void setCodeTimeout(int codeTimeout) {
        this.codeTimeout = codeTimeout;
    }

    public int getAccessTokenTimeout() {
        return accessTokenTimeout;
    }

    public void setAccessTokenTimeout(int accessTokenTimeout) {
        this.accessTokenTimeout = accessTokenTimeout;
    }

    public String getCookieName() {
        return cookieName;
    }

    public void setCookieName(String cookieName) {
        this.cookieName = cookieName;
    }
}