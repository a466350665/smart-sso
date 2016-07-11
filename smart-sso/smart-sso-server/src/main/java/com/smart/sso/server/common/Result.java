package com.smart.sso.server.common;

import java.io.Serializable;

/**
 * 返回结果
 * 
 * @param <T> 结果类型
 * @author Joe
 */
public class Result<T> implements Serializable {
	
	private static final long serialVersionUID = 4507869346123296527L;
	
	public final static String SUCESS = "success";// 成功
	public final static String ERROR = "error";// 错误

	/**
	 * 结果体
	 */
	private T data;

	/**
	 * 信息编号
	 */
	private String status = SUCESS;

	/**
	 * 信息
	 */
	private String message;

	public Result() {
	}

	public Result(T data) {
		this.data = data;
	}

	public Result(T data, String message) {
		this.data = data;
		this.message = message;
	}

	public Result(T data, String status, String message) {
		this.data = data;
		this.status = status;
		this.message = message;
	}

	public T getData() {
		return data;
	}

	public void setData(T data) {
		this.data = data;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}
}
