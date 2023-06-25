package com.smart.sso.server.exception;

import com.smart.core.enums.ResultEnum;
import com.smart.exception.CommonException;

/**
 * 业务逻辑异常
 * 
 * @author Joe
 */
public class ValidateException extends CommonException {

	private static final long serialVersionUID = -2678203134198782909L;

	public static final String MESSAGE = "验证异常";

	public ValidateException() {
		super(ResultEnum.VALIDATION_ERROR.getCode(), MESSAGE);
	}

	public ValidateException(String message) {
		super(ResultEnum.VALIDATION_ERROR.getCode(), message);
	}

    public ValidateException(int code, String message) {
		super(code, message);
    }
}