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
	private String account;

	public LoginUser(Integer userId, String account) {
		super();
		this.userId = userId;
		this.account = account;
	}

	public Integer getUserId() {
		return userId;
	}

	public void setUserId(Integer userId) {
		this.userId = userId;
	}

	public String getAccount() {
		return account;
	}

	public void setAccount(String account) {
		this.account = account;
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