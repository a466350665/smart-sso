package com.smart.mvc.exception;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.servlet.HandlerExceptionResolver;
import org.springframework.web.servlet.ModelAndView;

import com.alibaba.fastjson.JSON;
import com.smart.mvc.model.Result;
import com.smart.mvc.model.ResultCode;

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
		Result result = null;
		if (exception instanceof ApplicationException) {
			ApplicationException ae = (ApplicationException) exception;
			result = Result.create(ae.getCode()).setMessage(ae.getMessage());
		}
		else {
			result = Result.create(ResultCode.ERROR).setMessage("未知错误");
			logger.error(exception.getMessage(), exception);
		}

		response.setContentType("application/json;charset=UTF-8");
		response.setStatus(HttpStatus.OK.value());
		try {
			PrintWriter writer = response.getWriter();
			writer.write(JSON.toJSONString(result));
			writer.flush();
			writer.close();
		}
		catch (IOException ie) {
			logger.error("Failed to serialize the object to json for exception resolver!", ie);
		}
		return new ModelAndView();
	}
}
