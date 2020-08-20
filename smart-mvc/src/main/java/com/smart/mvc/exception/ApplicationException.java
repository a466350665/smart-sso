package com.smart.mvc.exception;

import com.smart.mvc.constant.ResultConstant;

/**
 * 应用异常
 * 
 * @author Joe
 */
public class ApplicationException extends RuntimeException{

	private static final long serialVersionUID = -2678203134198782909L;
	
	public static final String MESSAGE = "应用异常";

	protected int code;

	public ApplicationException() {
		super(MESSAGE);
	}

	public ApplicationException(String message) {
		super(message);
		this.code = ResultConstant.APPLICATION_ERROR;
	}
	
    public ApplicationException(int code, String message) {
        super(message);
        this.code = code;
    }

	public int getCode() {
		return code;
	}

	public void setCode(int code) {
		this.code = code;
	}
}
