package openjoe.smart.sso.client.filter;

import openjoe.smart.sso.base.constant.BaseConstant;
import openjoe.smart.sso.client.token.TokenStorage;
import org.springframework.core.annotation.Order;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * 单点退出Filter
 *
 * @author Joe
 */
@Order(10)
public class LogoutFilter extends AbstractClientFilter {

    private TokenStorage tokenStorage;

    public LogoutFilter(TokenStorage tokenStorage) {
        this.tokenStorage = tokenStorage;
    }

    @Override
    public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String accessToken = getLogoutParam(request);
        if (accessToken != null) {
            getTokenStorage().remove(accessToken);
            return false;
        }
        return true;
    }

    private String getLogoutParam(HttpServletRequest request) {
        return request.getHeader(BaseConstant.LOGOUT_PARAMETER_NAME);
    }

    public TokenStorage getTokenStorage() {
        return tokenStorage;
    }

    public void setTokenStorage(TokenStorage tokenStorage) {
        this.tokenStorage = tokenStorage;
    }
}