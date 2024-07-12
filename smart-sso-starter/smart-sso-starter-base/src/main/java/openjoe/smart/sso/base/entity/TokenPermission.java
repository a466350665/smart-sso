package openjoe.smart.sso.base.entity;

import java.util.List;
import java.util.Set;

/**
 * 已登录用户权限信息
 * 
 * @author Joe
 */
public class TokenPermission {
	
	// 用户菜单
	private List<TokenPermissionDTO> menuList;
	// 用户权限
	private Set<String> permissionSet;
	// 已添加权限控制，且用户没有分配的权限，以逗号分隔（用于前端显示控制）
	private String noPermissions;

	public List<TokenPermissionDTO> getMenuList() {
		return menuList;
	}

	public void setMenuList(List<TokenPermissionDTO> menuList) {
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