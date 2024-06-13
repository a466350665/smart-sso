package com.smart.sso.server.model;

import java.io.Serializable;

/**
 * 用户
 */
public class User implements Serializable {

    private static final long serialVersionUID = 10125567610925057L;

    /**
     * ID
     */
    private Integer id;
    /**
     * 姓名
     */
    private String name;
    /**
     * 登录名
     */
    private String username;
    /**
     * 密码
     */
    private String password;

    public User() {
        super();
    }

    public User(Integer id, String name, String username, String password) {
        super();
        this.id = id;
        this.name = name;
        this.username = username;
        this.password = password;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return this.password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}
