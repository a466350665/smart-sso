package com.smart.sso.client.constant;

/**
 * @author Joe
 */
public class Oauth2Constant {

	/**
	 * 用于保持请求和回调的状态，授权请求后原样带回给第三方。该参数可用于防止 csrf 攻击
	 */
	public static final String STATE = "state";
	
	/**
	 * 授权码名称
	 */
	public static final String AUTH_CODE = "code";
	
	/**
     * 获取授权码（app通过此方式由客户端代理转发http请求到服务端获取授权码）
     */
	public static final String AUTHORIZE_URL = "{0}/oauth2/authorize";
	
	/**
	 * 通过 code获取accessToken地址
	 */
	public static final String ACCESS_TOKEN_URL = "{0}/oauth2/access_token?appId={1}&appSecret={2}&" + AUTH_CODE + "={3}";
	
	/**
	 * 刷新accessToken地址
	 */
	public static final String REFRESH_TOKEN_URL = "{0}/oauth2/refresh_token?appId={1}&refreshToken={2}";
}
