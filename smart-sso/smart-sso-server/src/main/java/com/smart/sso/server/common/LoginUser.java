package com.smart.sso.server.common;

import java.io.Serializable;

/**
 * 登录成功用户对象
 * 
 * @author Joe
 */
public class LoginUser implements Serializable {

	private static final long serialVersionUID = 4507869346123296527L;

	// 登录成功ID
	private Integer userId;
	// 登录成功用户名
	private String userName;
	// 登录对象
	private Object profile;

	public LoginUser(Integer userId, String userName, Object profile) {
		super();
		this.userId = userId;
		this.userName = userName;
		this.profile = profile;
	}

	public Integer getUserId() {
		return userId;
	}

	public void setUserId(Integer userId) {
		this.userId = userId;
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

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		LoginUser other = (LoginUser) obj;
		if (userId == null) {
			if (other.userId != null)
				return false;
		}
		else if (!userId.equals(other.userId))
			return false;
		return true;
	}
}