package openjoe.smart.sso.base.constant;

/**
 * 单点登录基础常量
 *
 * @author Joe
 */
public class BaseConstant {

    /**
     * 服务端登录路径
     */
    public static final String LOGIN_PATH = "/sso/login";

    /**
     * 服务端退出路径
     */
    public static final String LOGOUT_PATH = "/sso/logout";

    /**
     * 认证路径
     */
    public static final String AUTH_PATH = "/sso/oauth2";

    /**
     * 权限路径
     */
    public static final String PERMISSION_PATH = "/sso/permission";

    /**
     * 服务端回调客户端地址参数名称
     */
    public static final String REDIRECT_URI = "redirectUri";

    /**
     * 服务端单点退出回调客户端退出参数名称
     */
    public static final String LOGOUT_PARAMETER_NAME = "logoutRequest";

    /**
     * 授权方式
     */
    public static final String GRANT_TYPE = "grantType";

    /**
     * 客户端ID
     */
    public static final String CLIENT_ID = "clientId";

    /**
     * 客户端密钥
     */
    public static final String CLIENT_SECRET = "clientSecret";

    /**
     * 访问token
     */
    public static final String ACCESS_TOKEN = "accessToken";

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
    public static final String ACCESS_TOKEN_PATH = BaseConstant.AUTH_PATH + "/access_token";

    /**
     * 刷新accessToken地址
     */
    public static final String REFRESH_TOKEN_PATH = BaseConstant.AUTH_PATH + "/refresh_token";
}
