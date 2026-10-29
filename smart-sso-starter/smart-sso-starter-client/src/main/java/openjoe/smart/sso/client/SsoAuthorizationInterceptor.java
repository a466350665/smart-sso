package openjoe.smart.sso.client;

import com.fasterxml.jackson.databind.ObjectMapper;
import openjoe.smart.sso.base.annotation.RequiresPermission;
import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.client.constant.ClientConstant;
import openjoe.smart.sso.client.util.ClientContextHolder;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * Enforces method-level stable permission codes for SSO-controlled requests.
 * Login enforcement remains the responsibility of SsoLoginFilter.
 */
public final class SsoAuthorizationInterceptor implements HandlerInterceptor {

    private final SsoUrlScopeMatcher scopeMatcher;
    private final ObjectMapper objectMapper = new ObjectMapper();

    public SsoAuthorizationInterceptor(SsoUrlScopeMatcher scopeMatcher) {
        this.scopeMatcher = scopeMatcher;
    }

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
            throws Exception {
        if (!scopeMatcher.isControlled(request.getServletPath()) || !(handler instanceof HandlerMethod handlerMethod)) {
            return true;
        }

        RequiresPermission requiresPermission = handlerMethod.getMethodAnnotation(RequiresPermission.class);
        if (requiresPermission == null) {
            // urlPatterns already enforced login; an unannotated method is intentionally login-only.
            return true;
        }

        if (ClientContextHolder.getUser() == null) {
            return responseJson(response, HttpServletResponse.SC_UNAUTHORIZED,
                    ClientConstant.NO_LOGIN, "未登录或已超时");
        }
        TokenPermission permission = ClientContextHolder.getPermission();
        if (permission == null || permission.getPermissionSet() == null
                || !permission.getPermissionSet().contains(requiresPermission.value())) {
            return responseJson(response, HttpServletResponse.SC_FORBIDDEN,
                    ClientConstant.NO_PERMISSION, "没有访问权限");
        }
        return true;
    }

    private boolean responseJson(HttpServletResponse response, int status, String code, String message)
            throws IOException {
        response.setContentType("application/json;charset=UTF-8");
        response.setStatus(status);
        try (PrintWriter writer = response.getWriter()) {
            writer.write(objectMapper.writeValueAsString(new Result<>(code, message)));
        }
        return false;
    }
}
