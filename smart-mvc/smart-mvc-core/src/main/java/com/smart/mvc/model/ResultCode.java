package com.smart.mvc.model;

public final class ResultCode {
	
	public final static String SUCCESS = "0000";// 成功

	// 通用错误以9开头
	public final static String ERROR = "9999";// 未知错误
	public final static String APPLICATION_ERROR = "9000";// 应用级错误
	public final static String VALIDATE_ERROR = "9001";// 参数验证错误
	public final static String SERVICE_ERROR = "9002";// 业务逻辑验证错误
	public final static String CACHE_ERROR = "9003";// 缓存访问错误
	public final static String DAO_ERROR = "9004";// 数据访问错误

	// SSO 用户授权出错
	public final static String SSO_TOKEN_ERROR = "1001"; // TOKEN未授权或已过期
	public final static String SSO_PERMISSION_ERROR = "1002"; // 没有访问权限
}
