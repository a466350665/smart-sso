package com.smart.sso.client.filter;

import com.smart.sso.client.constant.ClientConstant;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * 单点登出Filter
 * 
 * @author Joe
 */
public class LogoutFilter extends ClientFilter {

    @Override
    public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String accessToken = getLogoutParam(request);
        if (accessToken != null) {
            getTokenStorage().removeByAccessToken(accessToken);
            return false;
        }
        return true;
    }
    
    protected String getLogoutParam(HttpServletRequest request) {
    	return request.getHeader(ClientConstant.LOGOUT_PARAMETER_NAME);
    }
}