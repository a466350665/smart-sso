package openjoe.smart.sso.base.constant;

/**
 * @author Joe
 */
public class Oauth2Constant {

    /**
     * 授权方式
     */
    public static final String GRANT_TYPE = "grantType";

    /**
     * 应用唯一标识
     */
    public static final String APP_ID = "appId";

    /**
     * 应用密钥
     */
    public static final String APP_SECRET = "appSecret";

    /**
     * 刷新token
     */
    public static final String REFRESH_TOKEN = "refreshToken";

    /**
     * 授权码（授权码模式）
     */
    public static final String AUTH_CODE = "code";

    /**
     * 获取accessToken地址
     */
    public static final String ACCESS_TOKEN_URL = BaseConstant.AUTH_PATH + "/access_token";

    /**
     * 刷新accessToken地址
     */
    public static final String REFRESH_TOKEN_URL = BaseConstant.AUTH_PATH + "/refresh_token";
}
