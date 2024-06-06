package com.smart.sso.client.entity;

import java.beans.Transient;
import java.io.Serializable;

/**
 * 返回结果
 * 
 * @author Joe
 */
public class Result<T> implements Serializable {
    
    private static final long serialVersionUID = 1956544564021082972L;
    
    public static final int SUCCESS_CODE = 1;// 成功
    
    public static final int ERROR_CODE = 9999;// 未知错误

    /** 成功 */
    @SuppressWarnings("rawtypes")
    public static final Result SUCCESS = createSuccess();

	/**
	 * 结果体
	 */
	protected T data;

	/**
	 * 状态码
	 */
	protected int code;

	/**
	 * 信息
	 */
	protected String message;

	public Result() {
		super();
	}
	
    public static <T> Result<T> create() {
        return new Result<>();
    }

    public static <T> Result<T> create(int code) {
        Result<T> r = create();
        r.setCode(code);
        return r;
    }
    
    public static <T> Result<T> create(int code, String message) {
        Result<T> r = create(code);
        r.setMessage(message);
        return r;
    }
    
    @SuppressWarnings("unchecked")
	public static final <T> Result<T> success() {
		return SUCCESS;
	}

	public static <T> Result<T> createSuccess() {
		return create(SUCCESS_CODE);
	}
	
	public static <T> Result<T> createSuccess(T data) {
		Result<T> r = createSuccess();
		r.setData(data);
        return r;
	}

	public static <T> Result<T> createSuccess(T data, String message) {
		Result<T> r = createSuccess(data);
		r.setMessage(message);
        return r;
	}
	
	public static <T> Result<T> createError(String message) {
        return create(ERROR_CODE, message);
    }
	
	public T getData() {
		return data;
	}

	public Result<T> setData(T data) {
		this.data = data;
		return this;
	}

	public int getCode() {
		return code;
	}

	public Result<T> setCode(int code) {
		this.code = code;
		return this;
	}

	public String getMessage() {
		return message;
	}

	public Result<T> setMessage(String message) {
		this.message = message;
		return this;
	}
	
	@Transient
    public boolean isSuccess() {
        return SUCCESS_CODE == code;
    }
}
