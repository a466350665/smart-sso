package com.smart.ssm.model;


/**
 * 返回结果
 * 
 * @param <T> 结果类型
 * @author Joe
 */
public class JSONResult {

	/**
	 * 结果体
	 */
	private Object data;

	/**
	 * 状态码
	 */
	private String status = ResultCode.SUCCESS;

	/**
	 * 信息
	 */
	private String message;

	public JSONResult() {
	}

	public JSONResult(Object data) {
		this.data = data;
	}

	public JSONResult(Object data, String message) {
		this.data = data;
		this.message = message;
	}
	
	public JSONResult(String status, String message) {
		this.status = status;
		this.message = message;
	}

	public JSONResult(Object data, String status, String message) {
		this.data = data;
		this.status = status;
		this.message = message;
	}

	public Object getData() {
		return data;
	}

	public void setData(Object data) {
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
