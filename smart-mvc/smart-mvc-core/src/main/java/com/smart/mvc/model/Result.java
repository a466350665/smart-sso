package com.smart.mvc.model;


/**
 * 返回结果
 * 
 * @author Joe
 */
public class Result {

	/**
	 * 结果体
	 */
	protected Object data;

	/**
	 * 状态码
	 */
	protected String status;

	/**
	 * 信息
	 */
	protected String message;

	protected Result() {
	}

	private Result(String status, String message) {
		this.status = status;
		this.message = message;
	}

	public Object getData() {
		return data;
	}

	public Result setData(Object data) {
		this.data = data;
		return this;
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
	
	public boolean isSuccess() {
		return ResultCode.SUCCESS.equals(status);
	}
	
	public static Result create() {
		return create(ResultCode.SUCCESS, null);
	}

	public static Result create(String message) {
		return create(ResultCode.SUCCESS, message);
	}

	public static Result create(String status, String message) {
		return new Result(status, message);
	}
}
