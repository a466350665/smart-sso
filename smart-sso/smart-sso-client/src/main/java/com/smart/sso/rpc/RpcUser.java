package com.smart.sso.rpc;

import java.io.Serializable;

/**
 * RPC回传用户对象
 * 
 * @author Joe
 */
public class RpcUser implements Serializable {

	private static final long serialVersionUID = 4507869346123296527L;
	
	// 登录名
	private String account;

	public RpcUser(String account) {
		super();
		this.account = account;
	}

	public String getAccount() {
		return account;
	}

	public void setAccount(String account) {
		this.account = account;
	}
}