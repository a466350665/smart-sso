package openjoe.smart.sso.base.entity;

import java.util.List;
import java.util.Set;

/**
 * 已登录用户权限信息
 * 
 * @author Joe
 */
public class TokenPermission {

	/**
	 * 用户已分配的权限
	 */
	private Set<String> permissionSet;

	/**
	 * 用户未分配的权限（用于隐藏前端菜单和按钮）
	 */
	private Set<String> noPermissionSet;

	/**
	 * 用户已分配权限的菜单列表
	 */
	private List<TokenMenu> menuList;

	public TokenPermission() {
	}

	public TokenPermission(Set<String> permissionSet, Set<String> noPermissionSet) {
		this.permissionSet = permissionSet;
		this.noPermissionSet = noPermissionSet;
	}

	public TokenPermission(Set<String> permissionSet, Set<String> noPermissionSet, List<TokenMenu> menuList) {
		this.permissionSet = permissionSet;
		this.noPermissionSet = noPermissionSet;
		this.menuList = menuList;
	}

	public Set<String> getPermissionSet() {
		return permissionSet;
	}

	public void setPermissionSet(Set<String> permissionSet) {
		this.permissionSet = permissionSet;
	}

	public Set<String> getNoPermissionSet() {
		return noPermissionSet;
	}

	public void setNoPermissionSet(Set<String> noPermissionSet) {
		this.noPermissionSet = noPermissionSet;
	}

	public List<TokenMenu> getMenuList() {
		return menuList;
	}

	public void setMenuList(List<TokenMenu> menuList) {
		this.menuList = menuList;
	}
}