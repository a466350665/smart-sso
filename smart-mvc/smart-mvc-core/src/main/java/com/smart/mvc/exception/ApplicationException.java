package com.smart.mvc.exception;

import com.smart.mvc.model.ResultCode;

/**
 * 应用异常
 * 
 * @author Joe
 */
public class ApplicationException extends RuntimeException{

	private static final long serialVersionUID = -2678203134198782909L;
	
	public static final String MESSAGE = "系统服务出错！";

	protected String code = ResultCode.APPLICATION_ERROR;

	public ApplicationException() {
		super(MESSAGE);
	}

	public ApplicationException(String message) {
		super(message);
	}
	
	public ApplicationException(String code, String message) {
		super(message);
		this.code = code;
	}

	public ApplicationException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public ApplicationException(String code, String message, Throwable cause) {
		super(message, cause);
		this.code = code;
	}

	public ApplicationException(Throwable cause) {
		super(cause);
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}
}
