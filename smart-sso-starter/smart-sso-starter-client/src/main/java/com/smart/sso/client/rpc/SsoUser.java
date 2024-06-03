package com.smart.sso.client.rpc;

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
    private String username;

    public SsoUser(Integer id, String username) {
        super();
        this.id = id;
        this.username = username;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
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
