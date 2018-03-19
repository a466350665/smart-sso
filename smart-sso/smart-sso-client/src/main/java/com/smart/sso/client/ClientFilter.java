package com.smart.sso.client;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;

/**
 * Filter基类
 * 
 * @author Joe
 */
public abstract class ClientFilter extends ParamFilter implements Filter {
	
	// 匹配路径（? 匹配1个字符，* 匹配0个或多个字符，** 中的0个或多个目录）
	protected String pattern;

	public abstract boolean isAccessAllowed(HttpServletRequest request, HttpServletResponse response)
			throws IOException;

	protected boolean isAjaxRequest(HttpServletRequest request) {
		String requestedWith = request.getHeader("X-Requested-With");
		return requestedWith != null ? "XMLHttpRequest".equals(requestedWith) : false;
	}

	protected void responseJson(HttpServletResponse response, int code, String message) throws IOException {
		response.setContentType("application/json;charset=UTF-8");
		response.setStatus(HttpStatus.OK.value());
		PrintWriter writer = response.getWriter();
		writer.write(new StringBuilder().append("{\"code\":").append(code).append(",\"message\":\"").append(message)
				.append("\"}").toString());
		writer.flush();
		writer.close();
	}
	
	public void setPattern(String pattern) {
		this.pattern = pattern;
	}
	
	public String getPattern() {
		return pattern;
	}

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException {
	}

	@Override
	public void destroy() {
	}
}