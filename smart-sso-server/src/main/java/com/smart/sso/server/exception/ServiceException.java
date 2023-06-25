package com.smart.sso.server.exception;

import com.smart.core.enums.ResultEnum;
import com.smart.exception.CommonException;

/**
 * 业务逻辑异常
 * 
 * @author Joe
 */
public class ServiceException extends CommonException {

	private static final long serialVersionUID = -2678203134198782909L;

	public static final String MESSAGE = "业务逻辑异常";
	
	public ServiceException() {
		super(ResultEnum.ERROR.getCode(), MESSAGE);
	}

	public ServiceException(String message) {
		super(ResultEnum.ERROR.getCode(), message);
	}
	
    public ServiceException(int code, String message) {
		super(code, message);
    }
}