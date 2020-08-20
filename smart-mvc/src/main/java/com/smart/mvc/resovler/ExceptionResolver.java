package com.smart.mvc.resovler;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.servlet.HandlerExceptionResolver;
import org.springframework.web.servlet.ModelAndView;

import com.smart.mvc.constant.ResultConstant;
import com.smart.mvc.exception.ApplicationException;
import com.smart.mvc.model.Result;

/**
 * 统一异常处理
 * 
 * @author Joe
 */
public class ExceptionResolver implements HandlerExceptionResolver {

	private final Logger logger = LoggerFactory.getLogger(getClass());

	@Override
	public ModelAndView resolveException(HttpServletRequest request, HttpServletResponse response, Object handler,
			Exception exception) {
		Result<?> result;
		if (exception instanceof ApplicationException) {
			ApplicationException ae = (ApplicationException) exception;
			result = Result.create(ae.getCode(), ae.getMessage());
		}
		else {
			result = Result.create(ResultConstant.ERROR, "未知错误");
			logger.error(exception.getMessage(), exception);
		}

		response.setContentType("application/json;charset=UTF-8");
		response.setStatus(HttpStatus.OK.value());
		try {
			PrintWriter writer = response.getWriter();
			writer.write("{\"code\":" + result.getCode() + ",\"message\":\"" + result.getMessage() + "\"}");
			writer.flush();
			writer.close();
		}
		catch (IOException ie) {
			logger.error("Failed to serialize the object to json for exception resolver!", ie);
		}
		return new ModelAndView();
	}
}
