package com.smart.ssm.exception;

/**
 * 服务层异常
 * 
 * @author Joe
 */
public class ServiceException extends RuntimeException{

	private static final long serialVersionUID = -2678203134198782909L;
	
	public static final String MESSAGE = "系统服务出错！";

	public ServiceException() {
		super(MESSAGE);
	}

	public ServiceException(String s) {
		super(s);
	}

	public ServiceException(String message, Throwable cause) {
		super(message, cause);
	}

	public ServiceException(Throwable cause) {
		super(cause);
	}
}
