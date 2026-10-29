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
	 * 用户已分配权限的菜单
	 */
	private List<TokenMenu> menuList;

	public TokenPermission() {
	}

	public TokenPermission(Set<String> permissionSet, List<TokenMenu> menuList) {
		this.permissionSet = permissionSet;
		this.menuList = menuList;
	}

	public Set<String> getPermissionSet() {
		return permissionSet;
	}

	public void setPermissionSet(Set<String> permissionSet) {
		this.permissionSet = permissionSet;
	}

	public List<TokenMenu> getMenuList() {
		return menuList;
	}

	public void setMenuList(List<TokenMenu> menuList) {
		this.menuList = menuList;
	}
}
