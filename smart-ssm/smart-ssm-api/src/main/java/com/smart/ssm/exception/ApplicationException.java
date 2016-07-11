package com.smart.ssm.exception;

/**
 * 应用异常
 * 
 * @author Joe
 */
public class ApplicationException extends RuntimeException{

	private static final long serialVersionUID = -2678203134198782909L;
	
	public static final String MESSAGE = "系统服务出错！";

	public ApplicationException() {
		super(MESSAGE);
	}

	public ApplicationException(String s) {
		super(s);
	}

	public ApplicationException(String message, Throwable cause) {
		super(message, cause);
	}

	public ApplicationException(Throwable cause) {
		super(cause);
	}
}
