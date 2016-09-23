package com.smart.mvc.exception;

/**
 * 服务层异常
 * 
 * @author Joe
 */
public class RedisException extends RuntimeException{

	private static final long serialVersionUID = -2678203134198782909L;
	
	public static final String MESSAGE = "Redis服务出错！";

	public RedisException() {
		super(MESSAGE);
	}

	public RedisException(String s) {
		super(s);
	}

	public RedisException(String message, Throwable cause) {
		super(message, cause);
	}

	public RedisException(Throwable cause) {
		super(cause);
	}
}
