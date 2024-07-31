package openjoe.smart.sso.server.entity;

import openjoe.smart.sso.base.entity.TokenUser;

/**
 * Token存储信息
 *
 * @author Joe
 */
public class TokenContent extends CodeContent {

    private String accessToken;
    private String refreshToken;
    private TokenUser tokenUser;
    private String logoutUri;

    public TokenContent() {
        super();
    }

    public TokenContent(String accessToken, String refreshToken, TokenUser tokenUser, String logoutUri, String tgt, String clientId) {
        super(tgt, clientId);
        this.accessToken = accessToken;
        this.refreshToken = refreshToken;
        this.tokenUser = tokenUser;
        this.logoutUri = logoutUri;
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

    public TokenUser getTokenUser() {
        return tokenUser;
    }

    public void setTokenUser(TokenUser tokenUser) {
        this.tokenUser = tokenUser;
    }

    public String getLogoutUri() {
        return logoutUri;
    }

    public void setLogoutUri(String logoutUri) {
        this.logoutUri = logoutUri;
    }
}