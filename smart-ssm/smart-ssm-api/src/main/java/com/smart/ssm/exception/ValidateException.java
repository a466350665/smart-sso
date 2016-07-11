package com.smart.ssm.exception;

/**
 * 验证异常
 * 
 * @author Joe
 */
public class ValidateException extends RuntimeException {

	private static final long serialVersionUID = 5214146953001236471L;

	public static final String MESSAGE = "系统验证出错！";

	public ValidateException() {
		super(MESSAGE);
	}

	public ValidateException(String s) {
		super(s);
	}

	public ValidateException(String message, Throwable cause) {
		super(message, cause);
	}

	public ValidateException(Throwable cause) {
		super(cause);
	}
}
