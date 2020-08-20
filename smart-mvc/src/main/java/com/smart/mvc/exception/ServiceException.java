package com.smart.mvc.exception;

import com.smart.mvc.constant.ResultConstant;

/**
 * 业务逻辑异常
 * 
 * @author Joe
 */
public class ServiceException extends ApplicationException{

	private static final long serialVersionUID = -2678203134198782909L;
	
	public static final String MESSAGE = "业务逻辑异常";
	
	public ServiceException() {
		super(MESSAGE);
	}

	public ServiceException(String message) {
		super(message);
		this.code = ResultConstant.SERVICE_ERROR;
	}
	
    public ServiceException(int code, String message) {
        super(message);
        this.code = code;
    }
}
