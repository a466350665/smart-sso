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

	private Result() {
		super();
	}

	private Result(String status) {
		this.status = status;
	}

	public static Result create(String status) {
		return new Result(status);
	}

	/**
	 * Description:创建一个成功的结果体
	 * 
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:05:30
	 * @return
	 */
	public static Result createSuccessResult() {
		return create(ResultCode.SUCCESS);
	}

	/**
	 * Description:创建一个成功的结果体
	 * 
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:06:43
	 * @param data
	 *            将要返回的数据
	 * @param message
	 *            消息信息
	 * @return
	 */
	public static Result createSuccessResult(Object data, String message) {
		return createSuccessResult().setData(message).setMessage(message);
	}

	/**
	 * Description:创建一个默认的错误结果体
	 * 
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:07:09
	 * @return
	 */
	public static Result createErrorResult() {
		return create(ResultCode.ERROR);
	}

	/**
	 * Description:创建一个错误的结果体
	 * 
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:07:46
	 * @param data
	 *            将要返回的数据
	 * @param message
	 *            消息信息
	 * @return
	 */
	public static Result createErrorResult(Object data, String message) {
		return createErrorResult().setData(message).setMessage(message);
	}

	/**
	 * Description:判断该结果体是否是处理成功状态
	 * 
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:08:24
	 * @return
	 */
	public boolean isSuccess() {
		return ResultCode.SUCCESS.equals(this.status);
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

	public Result setStatus(String status) {
		this.status = status;
		return this;
	}

	public String getMessage() {
		return message;
	}

	public Result setMessage(String message) {
		this.message = message;
		return this;
	}
}
