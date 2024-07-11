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
     * 服务端回调客户端地址参数名称
     */
    public static final String REDIRECT_URI = "redirectUri";

    /**
     * 服务端单点退出回调客户端退出参数名称
     */
    public static final String LOGOUT_PARAMETER_NAME = "logoutRequest";
}
