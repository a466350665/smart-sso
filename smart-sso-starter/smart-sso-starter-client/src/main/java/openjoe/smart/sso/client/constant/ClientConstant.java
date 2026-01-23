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
    public static final String NO_LOGIN = "000010";

    /**
     * accessToken已失效，refreshToken未失效，用于通知客户端刷新token
     */
    public static final String NO_TOKEN = "000015";

    /**
     * 没有访问权限
     */
    public static final String NO_PERMISSION = "000020";
}
