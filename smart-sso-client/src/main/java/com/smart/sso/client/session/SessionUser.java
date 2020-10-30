package com.smart.sso.client.session;

import java.io.Serializable;

/**
 * session中用户信息
 * 
 * @author Joe
 */
public class SessionUser implements Serializable {

	private static final long serialVersionUID = -6755420347199782179L;

	// 登录成功userId
    private Integer id;
    // 登录名
    private String account;

    public SessionUser(Integer id, String account) {
        super();
        this.id = id;
        this.account = account;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getAccount() {
        return account;
    }

    public void setAccount(String account) {
        this.account = account;
    }
}
