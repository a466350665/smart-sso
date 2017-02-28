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
	protected Integer code;

	/**
	 * 信息
	 */
	protected String message;

	private Result() {
		super();
	}

	private Result(Integer code) {
		this.code = code;
	}

	public static Result create(Integer code) {
		return new Result(code);
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
		return code != null && code.equals(ResultCode.SUCCESS);
	}

	public Object getData() {
		return data;
	}

	public Result setData(Object data) {
		this.data = data;
		return this;
	}

	public Integer getCode() {
		return code;
	}

	public Result setCode(Integer code) {
		this.code = code;
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
