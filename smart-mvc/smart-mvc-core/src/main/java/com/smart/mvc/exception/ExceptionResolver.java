package com.smart.mvc.exception;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.HandlerExceptionResolver;
import org.springframework.web.servlet.ModelAndView;

/**
 * 统一异常处理
 * 
 * @author Joe
 */
public class ExceptionResolver implements HandlerExceptionResolver {

	private static final Logger LOGGER = LoggerFactory.getLogger(ExceptionResolver.class);

	private static final String ERROR_VIEW = "error";

	private String view = ERROR_VIEW;

	@Override
	public ModelAndView resolveException(HttpServletRequest request, HttpServletResponse response, Object handler,
			Exception exception) {
		LOGGER.error(exception.getMessage(), exception);
		Map<String, Object> model = new HashMap<String, Object>();
		model.put("exception", exception);
		return new ModelAndView(view, model);
	}

	public String getView() {
		return view;
	}

	public void setView(String view) {
		this.view = view;
	}
}
