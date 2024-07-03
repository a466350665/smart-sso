package openjoe.smart.sso.base.constant;

/**
 * 单点登录基础常量
 *
 * @author Joe
 */
public class BaseConstant {

    /**
     * 服务端登录地址
     */
    public static final String LOGIN_PATH = "/login";

    /**
     * 服务端回调客户端地址参数名称
     */
    public static final String REDIRECT_URI = "redirectUri";

    /**
     * 服务端单点退出回调客户端退出参数名称
     */
    public static final String LOGOUT_PARAMETER_NAME = "logoutRequest";
}
