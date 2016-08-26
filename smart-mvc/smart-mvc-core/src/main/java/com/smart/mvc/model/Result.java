package com.smart.mvc.model;

/**
 * <b>Description:结果体</b><br>
 * @author 唐海洋
 * @version 1.0 2016-8-17 下午5:37:44
 */
public class Result {

	private Integer status = ResultCode.SUCCESS;
	private Object attach;
	private String message;
	
	private Result() {
		super();
	}
	
	private Result(Integer status,Object data, String message) {
		this.status=status;
		this.attach=data;
		this.message=message;
	}
	
	/**
	 * Description:创建一个成功的结果体
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:05:30 
	 * @return
	 */
	public static Result createSuccessResult(){
		return createSuccessResult(null,null);
	}

	/**
	 * Description:创建一个成功的结果体
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:06:20 
	 * @param data 将要返回的数据
	 * @return
	 */
	public static Result createSuccessResult(Object data){
		return createSuccessResult(data,null);
	}
	
	/**
	 * Description:创建一个成功的结果体
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:06:43 
	 * @param data 将要返回的数据
	 * @param message 消息信息
	 * @return
	 */
	public static Result createSuccessResult(Object data,String message){
		return new Result(ResultCode.SUCCESS,data,message);
	}
	
	/**
	 * Description:创建一个默认的错误结果体
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:07:09 
	 * @return
	 */
	public static Result createErrorResult(){
		return createErrorResult(null,null);
	}
	
	/**
	 * Description:创建一个错误的结果体
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:07:09 
	 * @param message 消息信息
	 * @return
	 */
	public static Result createErrorResult(String message){
		return createErrorResult(null,message);
	}
	
	/**
	 * Description:创建一个错误的结果体
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:07:46 
	 * @param data 将要返回的数据
	 * @param message 消息信息
	 * @return
	 */
	public static Result createErrorResult(Object data,String message){
		return new Result(ResultCode.ERROR,data,message);
	}
	
	/**
	 * Description:判断该结果体是否是处理成功状态
	 * @author 唐海洋
	 * @Version 1.0 2016-8-24下午10:08:24 
	 * @return
	 */
	public boolean isSuccess(){
		return ResultCode.SUCCESS.equals(this.status);
	}

	public Object getAttach() {
		return attach;
	}

	public Result setAttach(Object attach) {
		this.attach = attach;
		return this;
	}

	public Integer getStatus() {
		return status;
	}

	public Result setStatus(Integer status) {
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