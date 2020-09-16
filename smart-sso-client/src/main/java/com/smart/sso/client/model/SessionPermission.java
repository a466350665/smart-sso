package com.smart.sso.client.model;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

import com.smart.sso.client.dto.RpcPermissionDto;

/**
 * 已登录用户权限信息
 * 
 * @author Joe
 */
public class SessionPermission implements Serializable {
	
	private static final long serialVersionUID = 7744061178030182892L;
	
	// 用户菜单
	private List<RpcPermissionDto> menuList;
	// 用户权限
	private Set<String> permissionSet;
	// 已添加权限控制，且用户没有分配的权限，以逗号分隔（用于前端显示控制）
	private String noPermissions;

	public List<RpcPermissionDto> getMenuList() {
		return menuList;
	}

	public void setMenuList(List<RpcPermissionDto> menuList) {
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
