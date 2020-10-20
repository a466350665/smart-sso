package com.smart.sso.client.dto;

import java.io.Serializable;

/**
 * 已登录用户信息
 * 
 * @author Joe
 */
public class SsoUser implements Serializable {

	private static final long serialVersionUID = 1764365572138947234L;

	// 登录成功userId
    private Integer id;
    // 登录名
    private String account;

    public SsoUser(Integer id, String account) {
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

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SsoUser other = (SsoUser)obj;
        if (id == null) {
            if (other.id != null)
                return false;
        } else if (!id.equals(other.id))
            return false;
        return true;
    }
}
