package openjoe.smart.sso.client.util;

import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.base.entity.TokenUser;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Optional;

/**
 * 客户端上下文
 *
 * @author Joe
 */
public class ClientContextHolder {

    private static final ThreadLocal<ClientContext> CLIENT_CONTENT = new ThreadLocal<>();

    public static void reset() {
        CLIENT_CONTENT.remove();
    }

    public static void create(HttpServletRequest request, HttpServletResponse response) {
        CLIENT_CONTENT.set(new ClientContext(request, response));
    }

    public static HttpServletRequest getRequest() {
        return Optional.ofNullable(get()).map(ClientContext::getRequest).orElse(null);
    }

    public static HttpServletResponse getResponse() {
        return Optional.ofNullable(get()).map(ClientContext::getResponse).orElse(null);
    }

    /**
     * 获取当前登录用户id
     *
     * @return
     */
    public static Long getUserId() {
        return Optional.ofNullable(getUser()).map(TokenUser::getId).orElse(null);
    }

    public static void setUser(TokenUser user) {
        ClientContext cc = get();
        if (cc != null) {
            cc.setUser(user);
        }
    }

    /**
     * 获取当前登录用户信息
     *
     * @return
     */
    public static TokenUser getUser() {
        return Optional.ofNullable(get()).map(ClientContext::getUser).orElse(null);
    }

    public static void setPermission(TokenPermission permission) {
        ClientContext cc = get();
        if (cc != null) {
            cc.setPermission(permission);
        }
    }

    /**
     * 获取当前登录用户权限信息
     *
     * @return
     */
    public static TokenPermission getPermission() {
        return Optional.ofNullable(get()).map(ClientContext::getPermission).orElse(null);
    }

    private static ClientContext get() {
        return CLIENT_CONTENT.get();
    }

    private static class ClientContext {
        private HttpServletRequest request;
        private HttpServletResponse response;
        private TokenUser user;
        private TokenPermission permission;

        public ClientContext(HttpServletRequest request, HttpServletResponse response) {
            this.request = request;
            this.response = response;
        }

        public HttpServletRequest getRequest() {
            return request;
        }

        public HttpServletResponse getResponse() {
            return response;
        }

        public void setRequest(HttpServletRequest request) {
            this.request = request;
        }

        public void setResponse(HttpServletResponse response) {
            this.response = response;
        }

        public TokenUser getUser() {
            return user;
        }

        public void setUser(TokenUser user) {
            this.user = user;
        }

        public TokenPermission getPermission() {
            return permission;
        }

        public void setPermission(TokenPermission permission) {
            this.permission = permission;
        }
    }
}