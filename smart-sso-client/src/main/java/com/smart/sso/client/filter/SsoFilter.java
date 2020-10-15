package com.smart.sso.client.filter;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLEncoder;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;
import com.smart.sso.client.constant.SsoConstant;
import com.smart.sso.client.dto.RpcUserDto;
import com.smart.sso.client.model.Result;
import com.smart.sso.client.model.SessionUser;
import com.smart.sso.client.util.HttpRequestUtils;
import com.smart.sso.client.util.SessionUtils;

/**
 * 单点登录ticket验证Filter
 * 
 * @author Joe
 */
public class SsoFilter extends ClientFilter {
    
    private final Logger logger = LoggerFactory.getLogger(getClass());

	@Override
	public boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (SessionUtils.getUser(request) != null) {
            return true;
        }
        String ticket = request.getParameter(SsoConstant.TICKET_PARAMETER_NAME);
        if (ticket != null) {
            RpcUserDto rpcUser = validateTicket(ticket);
            if (rpcUser != null) {
                // 存储sessionUser
                SessionUtils.setUser(request, new SessionUser(rpcUser.getId(), rpcUser.getAccount()));
            }
            // 为去掉URL中token参数，再跳转一次当前Service
            redirectRemoveTicketService(request, response);
        }
        else {
            redirectLogin(request, response);
        }
        return false;
	}
	
    private RpcUserDto validateTicket(String ticket) {
        String user = HttpRequestUtils
            .get(new StringBuilder().append(ssoServerUrl).append("/validate?ticket=").append(ticket).toString());
        if (user == null || user.isEmpty()) {
            logger.error("ticket validate exception, return null. ticket:{}", ticket);
            return null;
        }
        Result<RpcUserDto> result = JSONObject.parseObject(user, new TypeReference<Result<RpcUserDto>>() {});
        if (!result.isSuccess()) {
            logger.error("ticket validate exception, ticket:{}, message:{}", ticket, result.getMessage());
            return null;
        }
        return result.getData();
    }

	/**
	 * 跳转至服务端登录
	 * 
	 * @param request
	 * @param response
	 * @throws IOException
	 */
	private void redirectLogin(HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (isAjaxRequest(request)) {
			responseJson(response, SsoConstant.NO_LOGIN, "未登录或已超时");
		}
		else {
			String ssoLoginUrl = new StringBuilder().append(ssoServerUrl)
					.append("/login?service=").append(URLEncoder.encode(getService(request), "utf-8")).toString();

			response.sendRedirect(ssoLoginUrl);
		}
	}

    /**
     * 去除返回地址中的票据参数
     * 
     * @param request
     * @return
     * @throws IOException
     */
    private void redirectRemoveTicketService(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String service = getService(request);
        service = service.substring(0, service.indexOf(SsoConstant.TICKET_PARAMETER_NAME) - 1);
        response.sendRedirect(service);
    }
	
    /**
     * 获取当前请求地址
     * 
     * @param request
     * @return
     */
	private String getService(HttpServletRequest request) {
		return new StringBuilder().append(request.getRequestURL())
				.append(request.getQueryString() == null ? "" : "?" + request.getQueryString()).toString();
	}
	
	protected boolean isAjaxRequest(HttpServletRequest request) {
        String requestedWith = request.getHeader("X-Requested-With");
        return requestedWith != null ? "XMLHttpRequest".equals(requestedWith) : false;
    }
    
    protected void responseJson(HttpServletResponse response, int code, String message) throws IOException {
        response.setContentType("application/json;charset=UTF-8");
        response.setStatus(200);
        PrintWriter writer = response.getWriter();
        writer.write(JSON.toJSONString(Result.create(code, message)));
        writer.flush();
        writer.close();
    }
}