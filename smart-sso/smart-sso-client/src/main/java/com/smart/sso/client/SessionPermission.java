package com.smart.sso.client;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

import com.smart.sso.rpc.RpcPermission;

/**
 * 已登录用户权限信息
 * 
 * @author Joe
 */
public class SessionPermission implements Serializable {
	
	private static final long serialVersionUID = 7744061178030182892L;
	
	// 用户菜单
	private List<RpcPermission> menuList;
	// 用户权限
	private Set<String> permissionSet;
	// 用户没有的权限
	private String noPermissions;

	public List<RpcPermission> getMenuList() {
		return menuList;
	}

	public void setMenuList(List<RpcPermission> menuList) {
		this.menuList = menuList;
	}

	public Set<String> getPermissionSet() {
		return permissionSet;
	}

	public void setPermissionSet(Set<String> permissionSet) {
		this.permissionSet = permissionSet;
	}

	public String getNoPermissions() {
		return noPermissions;
	}

	public void setNoPermissions(String noPermissions) {
		this.noPermissions = noPermissions;
	}
}
