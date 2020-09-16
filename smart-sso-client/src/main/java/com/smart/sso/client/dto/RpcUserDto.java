package com.smart.sso.client.dto;

import java.io.Serializable;

/**
 * RPC回传用户对象
 * 
 * @author Joe
 */
public class RpcUserDto implements Serializable {

    private static final long serialVersionUID = 4507869346123296527L;

    // 登录成功userId
    private Integer id;
    // 登录名
    private String account;

    public RpcUserDto(Integer id, String account) {
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
        RpcUserDto other = (RpcUserDto)obj;
        if (id == null) {
            if (other.id != null)
                return false;
        } else if (!id.equals(other.id))
            return false;
        return true;
    }
}