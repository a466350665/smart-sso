package openjoe.smart.sso.server.entity;

import openjoe.smart.sso.base.entity.Userinfo;

/**
 * Token存储信息
 *
 * @author Joe
 */
public class TokenContent extends CodeContent {

    private String accessToken;
    private String refreshToken;
    private Userinfo userinfo;
    private String appId;

    public TokenContent() {
        super();
    }

    public TokenContent(String accessToken, String refreshToken, Userinfo userinfo, String appId, String tgt, String redirectUri) {
        super(tgt, redirectUri);
        this.accessToken = accessToken;
        this.refreshToken = refreshToken;
        this.userinfo = userinfo;
        this.appId = appId;
    }

    public String getAccessToken() {
        return accessToken;
    }

    public void setAccessToken(String accessToken) {
        this.accessToken = accessToken;
    }

    public String getRefreshToken() {
        return refreshToken;
    }

    public void setRefreshToken(String refreshToken) {
        this.refreshToken = refreshToken;
    }

    public Userinfo getUserinfo() {
        return userinfo;
    }

    public void setUserinfo(Userinfo userinfo) {
        this.userinfo = userinfo;
    }

    public String getAppId() {
        return appId;
    }

    public void setAppId(String appId) {
        this.appId = appId;
    }
}