package com.smart.sso.server.entity;

/**
 * 已登录用户信息
 * 
 * @author Joe
 */
public class ServerUser {

	private static final long serialVersionUID = 1764365572138947234L;

	// 登录成功userId
    private Integer id;
    // 登录名
    private String username;

    public ServerUser() {
        super();
    }

    public ServerUser(Integer id, String username) {
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
}
