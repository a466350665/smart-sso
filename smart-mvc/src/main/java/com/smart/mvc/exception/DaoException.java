package com.smart.mvc.exception;

import com.smart.mvc.model.ResultCode;

/**
 * 数据访问异常
 * 
 * @author Joe
 */
public class DaoException extends ApplicationException {

	private static final long serialVersionUID = -7980532772047897013L;

	public static final String MESSAGE = "数据访问异常";

	public DaoException() {
		super(MESSAGE);
	}

	public DaoException(String message) {
		super(message);
		this.code = ResultCode.DAO_ERROR;
	}
	
	public DaoException(int code, String message) {
		super(message);
		this.code = code;
	}

	public DaoException(String message, Throwable cause) {
		super(message, cause);
		this.code = ResultCode.DAO_ERROR;
	}
	
	public DaoException(int code, String message, Throwable cause) {
		super(message, cause);
		this.code = code;
	}

	public DaoException(Throwable cause) {
		super(cause);
		this.code = ResultCode.DAO_ERROR;
	}
}
