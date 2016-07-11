package com.smart.base.model;

import com.smart.ssm.model.PersistentObject;

/**
 * 管理员
 * 
 * @author Joe
 */
public class User extends PersistentObject {

	private static final long serialVersionUID = 1106412532325860697L;

	/** 登录名 */
	private String account;

	public String getAccount() {
		return account;
	}

	public void setAccount(String account) {
		this.account = account;
	}
}
