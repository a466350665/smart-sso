package openjoe.smart.sso.client.constant;

/**
 * @author Joe
 */
public class ClientConstant {

    /**
     * 模糊匹配后缀
     */
    public static final String URL_FUZZY_MATCH = "/*";

    /**
     * 未登录或已过期（无token，或者accessToken和refreshToken均已失效）
     */
    public static final int NO_LOGIN = 10;

    /**
     * accessToken已失效，refreshToken未失效，用于通知客户端刷新token
     */
    public static final int NO_TOKEN = 15;

    /**
     * 没有访问权限
     */
    public static final int NO_PERMISSION = 20;
}
