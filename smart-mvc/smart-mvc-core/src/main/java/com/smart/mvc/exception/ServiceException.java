package com.smart.mvc.exception;

import com.smart.mvc.model.ResultCode;

/**
 * 服务层异常
 * 
 * @author Joe
 */
public class ServiceException extends ApplicationException{

	private static final long serialVersionUID = -2678203134198782909L;
	
	public static final String MESSAGE = "系统服务出错！";
	
	public ServiceException() {
		super(MESSAGE);
	}

	public ServiceException(String message) {
		super(message);
		this.code = ResultCode.SERVICE_ERROR;
	}
	
	public ServiceException(int code, String message) {
		super(message);
		this.code = code;
	}

	public ServiceException(String message, Throwable cause) {
		super(message, cause);
		this.code = ResultCode.SERVICE_ERROR;
	}
	
	public ServiceException(int code, String message, Throwable cause) {
		super(message, cause);
		this.code = code;
	}

	public ServiceException(Throwable cause) {
		super(cause);
		this.code = ResultCode.SERVICE_ERROR;
	}
}
