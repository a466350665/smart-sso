package openjoe.smart.sso.client.filter;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import openjoe.smart.sso.base.constant.BaseConstant;

import java.io.IOException;

/**
 * 单点退出Filter
 *
 * @author Joe
 */
public class LogoutFilter extends AbstractClientFilter {

    @Override
    public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String accessToken = getLogoutParam(request);
        if (accessToken != null) {
            getTokenStorage().remove(accessToken);
            return false;
        }
        return true;
    }

    protected String getLogoutParam(HttpServletRequest request) {
        return request.getHeader(BaseConstant.LOGOUT_PARAMETER_NAME);
    }
}