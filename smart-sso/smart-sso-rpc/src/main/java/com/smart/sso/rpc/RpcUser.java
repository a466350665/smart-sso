package com.smart.sso.rpc;

import java.io.Serializable;

/**
 * 通信传输对象
 * 
 * @author Joe
 */
public class RpcUser implements Serializable {

	private static final long serialVersionUID = 4507869346123296527L;

	// 登录成功用户名
	private String userName;
	// 登录对象
	private Object profile;

	public RpcUser(String userName, Object profile) {
		super();
		this.userName = userName;
		this.profile = profile;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public Object getProfile() {
		return profile;
	}

	public void setProfile(Object profile) {
		this.profile = profile;
	}
}