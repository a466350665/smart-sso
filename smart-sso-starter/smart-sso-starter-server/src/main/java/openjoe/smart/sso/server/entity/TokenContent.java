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
    private String appKey;

    public TokenContent() {
        super();
    }

    public TokenContent(String accessToken, String refreshToken, Userinfo userinfo, String appKey, String tgt, String redirectUri) {
        super(tgt, redirectUri);
        this.accessToken = accessToken;
        this.refreshToken = refreshToken;
        this.userinfo = userinfo;
        this.appKey = appKey;
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

    public String getAppKey() {
        return appKey;
    }

    public void setAppKey(String appKey) {
        this.appKey = appKey;
    }
}