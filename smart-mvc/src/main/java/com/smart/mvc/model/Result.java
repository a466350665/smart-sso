package com.smart.mvc.model;

import java.beans.Transient;
import java.io.Serializable;

import com.smart.mvc.constant.ResultConstant;

/**
 * 返回结果
 * 
 * @author Joe
 */
public class Result<T> implements Serializable {
    
    private static final long serialVersionUID = 1956544564021082972L;

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

	private Result() {
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
		return create(ResultConstant.SUCCESS);
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
        return ResultConstant.SUCCESS == code;
    }
}
