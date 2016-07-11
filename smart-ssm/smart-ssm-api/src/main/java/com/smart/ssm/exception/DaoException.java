package com.smart.ssm.exception;

/**
 * 数据访问异常
 * 
 * @author Joe
 */
public class DaoException extends RuntimeException {

	private static final long serialVersionUID = -7980532772047897013L;

	public static final String MESSAGE = "数据访问出错！";

	public DaoException() {
		super(MESSAGE);
	}

	public DaoException(String s) {
		super(s);
	}

	public DaoException(String message, Throwable cause) {
		super(message, cause);
	}

	public DaoException(Throwable cause) {
		super(cause);
	}
}
