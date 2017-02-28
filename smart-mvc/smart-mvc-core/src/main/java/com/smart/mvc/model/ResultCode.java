package com.smart.mvc.model;

public final class ResultCode {
	
	public final static int SUCCESS = 1;// 成功

	// 通用错误以9开头
	public final static int ERROR = 9999;// 未知错误
	public final static int APPLICATION_ERROR = 9000;// 应用级错误
	public final static int VALIDATE_ERROR = 9001;// 参数验证错误
	public final static int SERVICE_ERROR = 9002;// 业务逻辑验证错误
	public final static int CACHE_ERROR = 9003;// 缓存访问错误
	public final static int DAO_ERROR = 9004;// 数据访问错误

	// SSO 用户授权出错
	public final static int SSO_TOKEN_ERROR = 1001; // TOKEN未授权或已过期
	public final static int SSO_PERMISSION_ERROR = 1002; // 没有访问权限
}
