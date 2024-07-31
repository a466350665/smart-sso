package openjoe.smart.sso.base.entity;

/**
 * 服务端回传凭证Token对象
 *
 * @author Joe
 */
public class Token {

    /**
     * 调用凭证
     */
    private String accessToken;
    /**
     * 调用凭证accessToken超时时间，单位（秒）
     */
    private int expiresIn;
    /**
     * 刷新凭证
     */
    private String refreshToken;
    /**
     * 刷新凭证refreshToken超时时间，单位（秒）
     */
    private int refreshExpiresIn;
    /**
     * 用户信息
     */
    private TokenUser tokenUser;
    /**
     * 用户权限信息
     */
    private TokenPermission tokenPermission;

    public Token() {
        super();
    }

    public Token(String accessToken, int expiresIn, String refreshToken, int refreshExpiresIn, TokenUser tokenUser, TokenPermission tokenPermission) {
        super();
        this.accessToken = accessToken;
        this.expiresIn = expiresIn;
        this.refreshToken = refreshToken;
        this.refreshExpiresIn = refreshExpiresIn;
        this.tokenUser = tokenUser;
        this.tokenPermission = tokenPermission;
    }

    public String getAccessToken() {
        return accessToken;
    }

    public void setAccessToken(String accessToken) {
        this.accessToken = accessToken;
    }

    public int getExpiresIn() {
        return expiresIn;
    }

    public void setExpiresIn(int expiresIn) {
        this.expiresIn = expiresIn;
    }

    public String getRefreshToken() {
        return refreshToken;
    }

    public void setRefreshToken(String refreshToken) {
        this.refreshToken = refreshToken;
    }

    public int getRefreshExpiresIn() {
        return refreshExpiresIn;
    }

    public void setRefreshExpiresIn(int refreshExpiresIn) {
        this.refreshExpiresIn = refreshExpiresIn;
    }

    public TokenUser getTokenUser() {
        return tokenUser;
    }

    public void setTokenUser(TokenUser tokenUser) {
        this.tokenUser = tokenUser;
    }

    public TokenPermission getTokenPermission() {
        return tokenPermission;
    }

    public void setTokenPermission(TokenPermission tokenPermission) {
        this.tokenPermission = tokenPermission;
    }
}