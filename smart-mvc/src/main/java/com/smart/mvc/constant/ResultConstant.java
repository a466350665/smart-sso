package com.smart.mvc.constant;

/**
 * 常用结果返回码
 * 
 * @author Joe
 */
public class ResultConstant {

	public static final int SUCCESS = 1;// 成功

	public static final int ERROR = 9999;// 未知错误
	
	public static final int APPLICATION_ERROR = 1000;// 应用级错误
	
	public static final int VALIDATE_ERROR = 2000;// 参数验证错误
	public static final int NO_LOGIN = 2100;// 未登录
	public static final int NO_PERMISSION = 2200;// 没有权限
    
	public static final int SERVICE_ERROR = 3000;// 业务逻辑验证错误
	
	public static final int CACHE_ERROR = 4000;// 缓存访问错误
	
	public static final int DAO_ERROR = 5000;// 数据访问错误
}
