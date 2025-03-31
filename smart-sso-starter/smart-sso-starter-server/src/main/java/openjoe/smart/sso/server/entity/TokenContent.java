package openjoe.smart.sso.server.entity;

/**
 * Token存储信息
 *
 * @author Joe
 */
public class TokenContent extends CodeContent {

    private String accessToken;
    private String refreshToken;
    private Long userId;
    private String logoutUri;

    public TokenContent() {
        super();
    }

    public TokenContent(String accessToken, String refreshToken, Long userId, String logoutUri, String tgt, String clientId) {
        super(tgt, clientId);
        this.accessToken = accessToken;
        this.refreshToken = refreshToken;
        this.userId = userId;
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

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getLogoutUri() {
        return logoutUri;
    }

    public void setLogoutUri(String logoutUri) {
        this.logoutUri = logoutUri;
    }
}