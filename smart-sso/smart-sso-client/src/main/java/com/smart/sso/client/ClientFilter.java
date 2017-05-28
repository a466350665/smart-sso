package com.smart.sso.client;

import com.alibaba.fastjson.JSON;
import com.smart.mvc.config.ConfigUtils;
import com.smart.mvc.exception.ServiceException;
import com.smart.mvc.model.Result;
import com.smart.mvc.util.SpringUtils;
import com.smart.mvc.util.StringUtils;
import com.smart.sso.rpc.AuthenticationRpcService;
import org.springframework.http.HttpStatus;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;

/**
 * 单点登录权限系统Filter基类
 * 
 * @author Joe
 */
public abstract class ClientFilter implements Filter {

	// 单点登录服务端URL
	protected String ssoServerUrl;
	// 当前应用关联权限系统的应用编码
	protected String ssoAppCode;
	// 单点登录服务端提供的RPC服务，由Spring容器注入
	protected AuthenticationRpcService authenticationRpcService;

	// 排除拦截
	protected List<String> excludeList = null;

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
		if (StringUtils.isBlank(ssoServerUrl = ConfigUtils.getProperty("sso.server.url"))) {
			throw new IllegalArgumentException("ssoServerUrl不能为空");
		}

		if (StringUtils.isBlank(ssoAppCode = ConfigUtils.getProperty("sso.app.code"))) {
			throw new IllegalArgumentException("ssoAppCode不能为空");
		}

		if ((authenticationRpcService = SpringUtils.getBean(AuthenticationRpcService.class)) == null) {
			throw new IllegalArgumentException("authenticationRpcService注入失败");
		}
		
		String excludes = filterConfig.getInitParameter("excludes");
		if (StringUtils.isBlank(excludes)) {
			excludes = "";
		}

		excludeList = Arrays.asList(excludes.split(","));
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		HttpServletRequest httpRequest = (HttpServletRequest) request;

		for (String ignore : excludeList) {
			//判断请求地址是否为以忽略地址为后缀，相同者略过检查
			if (httpRequest.getServletPath().endsWith(ignore)) {
				chain.doFilter(request, response);

				return ;
			}
		}


		HttpServletResponse httpResponse = (HttpServletResponse) response;
		try {
			doFilter(httpRequest, httpResponse, chain);
		} catch (ServiceException e) {
			httpResponse.setContentType("application/json;charset=UTF-8");
			httpResponse.setStatus(HttpStatus.OK.value());
			PrintWriter writer = httpResponse.getWriter();
			writer.write(JSON.toJSONString(Result.create(e.getCode()).setMessage(e.getMessage())));
			writer.flush();
			writer.close();
		}
	}

	public abstract void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
			throws IOException, ServletException, ServiceException;

	@Override
	public void destroy() {
	}
}